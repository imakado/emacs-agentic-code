;;; agentic-code-helm.el --- Helm interface for Agentic Code sessions -*- lexical-binding: t -*-

;; Copyright (C) 2025 imakado

;; Author: imakado <imakado@gmail.com>
;; Version: 0.1.0
;; Keywords: tools, convenience, ai, helm
;; URL: https://github.com/imakado/emacs-agentic-code

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a Helm-based interface for managing and selecting
;; active Claude Code sessions in Agentic Code.
;;
;; Features:
;; - List all active Claude Code sessions
;; - Display sessions sorted by last access time
;; - Preview session content with persistent action
;; - Restore associated edit buffers when switching sessions
;; - Automatic cleanup of dead sessions
;; - Projectile integration for copying file paths to clipboard
;;
;; Usage:
;;
;; `M-x agentic-code-helm-select-session' to select a session.
;;
;; The helm interface will show:
;; - Project directory name
;; - Last access timestamp
;;
;; When selecting a session:
;; - The vterm buffer will be displayed
;; - Any associated edit buffer will be shown in a split window
;; - Previous window configuration will be restored if available
;;
;; Projectile Integration:
;;
;; `M-x agentic-code-helm-projectile-send-files' to send file references to Claude Code.
;;
;; - Mark files with C-SPC or mark all with M-a
;; - Press RET to send file reference(s) to Claude Code
;; - Multiple file references are joined with newlines

;;; Code:

;;;; Dependencies

(require 'cl-lib)
(require 'helm)
(require 'dash)
(require 'f)
(require 'let-alist)
(eval-when-compile (require 'subr-x)) ;; For when-let*

;; Optional dependencies for projectile integration
(declare-function projectile-project-p "projectile" ())
(declare-function projectile-project-root "projectile" ())
(declare-function projectile-current-project-files "projectile" ())

;;;; Forward declarations

;; Declare functions from agentic-code.el to avoid byte-compile warnings
(declare-function agentic-code--session-cleanup-dead! "agentic-code")
(declare-function agentic-code--session-update-timestamp! "agentic-code")
(declare-function agentic-code--session-save-window-config! "agentic-code")
(declare-function agentic-code--get-claude-file-reference-string "agentic-code")
(declare-function agentic-code-send-claude-file-reference-dwim "agentic-code")

;; Declare variables from agentic-code.el
(defvar agentic-code--sessions)
(defvar agentic-code--current-edit-buffer)

;;;; Session Selection

(cl-defun agentic-code--helm-session-candidates ()
  "Generate helm candidates from active Claude Code sessions.
Returns a list of (DISPLAY . SESSION-ENTRY) pairs suitable for helm."
  ;; Check if agentic-code is loaded
  (cond
   ((not (featurep 'agentic-code))
    (error "Agentic Code is not loaded. Please require 'agentic-code first"))
   (t
    ;; Clean up dead sessions first
    (agentic-code--session-cleanup-dead!)))

  ;; Sort sessions by last-displayed timestamp and create candidates
  (cl-loop for (root-dir . session-info) in agentic-code--sessions
           collect `(,root-dir . ,session-info) into session-list
           finally return
           (cl-loop for session in (cl-sort session-list
                                            (lambda (a b)
                                              (> (or (let-alist (cdr a) .last-displayed) 0)
                                                 (or (let-alist (cdr b) .last-displayed) 0))))
                    for root-dir = (car session)
                    for session-info = (cdr session)
                    collect (let-alist session-info
                              `(,(format "%-50s %s"
                                         (f-filename root-dir)
                                         (cond
                                          (.last-displayed
                                           (format-time-string "%Y-%m-%d %H:%M:%S" .last-displayed))
                                          (t "Never")))
                                . ,session)))))

(cl-defun agentic-code--helm-display-multiple-sessions-vertical (candidates)
  "Display multiple selected sessions in evenly-sized vertical splits.
CANDIDATES is a list of marked session entries from Helm."
  ;; Clear existing windows
  (delete-other-windows)

  ;; Create a list to track root directories for later updates
  (let* ((displayed-sessions '())
         (first-window (selected-window))
         (session-count 0))

    ;; Display each session
    (cl-loop for candidate in candidates
             for session-entry = (cond
                                  ((consp candidate) (cdr candidate))
                                  (t candidate))
             when session-entry
             do (-let* (((root-dir . session-info) session-entry))
                  (let-alist session-info
                    (when (and .vterm-buffer (buffer-live-p .vterm-buffer))
                      ;; Split window for all but the first session
                      (when (> session-count 0)
                        (split-window-right))
                      ;; Display buffer
                      (set-window-buffer (selected-window) .vterm-buffer)
                      ;; Move to next window
                      (other-window 1)
                      ;; Track displayed session
                      (push root-dir displayed-sessions)
                      (cl-incf session-count)))))

    ;; Balance all windows evenly
    (when (> session-count 0)
      (balance-windows))

    ;; Return to first window
    (select-window first-window)

    ;; Update timestamps and save window configurations for all displayed sessions
    (cl-loop for root-dir in displayed-sessions
             do (progn
                  (agentic-code--session-update-timestamp! root-dir)
                  (agentic-code--session-save-window-config! root-dir)))

    ;; Report success
    (message "Displayed %d sessions in vertical splits" session-count)))

(cl-defun agentic-code--helm-display-session-buffers (session-entry)
  "Display buffers for the selected SESSION-ENTRY.
If multiple candidates are marked, display them in splits.
Otherwise, display single session with its window configuration.
SESSION-ENTRY is a (ROOT-DIR . SESSION-INFO) cons cell."
  (let* ((marked-candidates (helm-marked-candidates))
         (candidates-count (length marked-candidates)))
    (cond
     ;; 複数の候補がマークされている場合
     ((> candidates-count 1)
      ;; 既存の複数表示ロジックを使用
      (agentic-code--helm-display-multiple-sessions-vertical marked-candidates))

     ;; 単一の候補の場合（従来の動作）
     (t
      (-let* (((root-dir . session-info) session-entry))
        (let-alist session-info
          (cond
           ;; Restore saved window configuration if available
           ((when-let* ((config .window-config)
                        ((window-configuration-p config)))
              (set-window-configuration config)
              (agentic-code--session-update-timestamp! root-dir)
              t))

           ;; Display vterm buffer and optionally edit buffer
           ((when-let* ((buffer .vterm-buffer)
                        ((buffer-live-p buffer)))
              (display-buffer buffer)
              (select-window (get-buffer-window buffer))

              ;; Display associated edit buffer if exists
              (when-let* ((edit-buffer (buffer-local-value 'agentic-code--current-edit-buffer buffer))
                          ((buffer-live-p edit-buffer))
                          (edit-window (split-window-sensibly)))
                (display-buffer edit-buffer)
                ;; (set-window-buffer edit-window edit-buffer)
                ;; (select-window edit-window)
                )

              ;; Update session state
              (agentic-code--session-update-timestamp! root-dir)
              (agentic-code--session-save-window-config! root-dir)
              t))

           (t (error "Session buffer is no longer alive")))))))))

(defvar agentic-code-helm-session-source
  (helm-build-sync-source "Claude Code Sessions"
    :candidates 'agentic-code--helm-session-candidates
    :action '(("Switch to session(s)" . agentic-code--helm-display-session-buffers))
    :persistent-action 'agentic-code--helm-display-session-buffers
    :persistent-help "Preview session"
    :marked-with-props nil)
  "Helm source for Claude Code sessions.")

;;;###autoload
(cl-defun agentic-code-helm-select-session ()
  "Select and switch to a Claude Code session using helm.
Shows all active sessions sorted by last access time.
If the selected session has an associated edit buffer, both
the vterm and edit buffers will be displayed."
  (interactive)
  (unless (featurep 'agentic-code)
    (error "Agentic Code is not loaded. Please require 'agentic-code first"))
  (cond
   ((null agentic-code--sessions)
    (message "No active Claude Code sessions found"))
   (t
    (helm :sources 'agentic-code-helm-session-source
          :buffer "*helm claude code sessions*"
          :truncate-lines t))))

;;;; Projectile Integration

(defgroup agentic-code-helm-projectile nil
  "Projectile integration for sending file references to Claude Code via Helm."
  :group 'agentic-code-helm
  :prefix "agentic-code-helm-projectile-")


(cl-defun agentic-code-helm-projectile--send-file-references (_candidate)
  "Send selected file reference(s) to Claude Code.
CANDIDATE is the selected file path.
If multiple files are marked, send them as separate references."
  (let* ((marked-candidates (helm-marked-candidates))
         (paths (cl-loop for cand in marked-candidates
                         collect
                         (s-trim
                          (agentic-code--get-claude-file-reference-string
                           :file-path cand
                           :region-support-p nil))))
         (result (mapconcat 'identity paths "\n")))
    (agentic-code-send-claude-file-reference-dwim
     :file-reference-string result)))

(cl-defun agentic-code-helm-projectile--candidates ()
  "Generate candidates for projectile files in current project."
  (cond
   ((not (projectile-project-p))
    nil)
   (t
    (projectile-current-project-files))))

(defvar agentic-code-helm-projectile-source
  (helm-build-sync-source "Projectile Files (Send to Claude Code)"
    :candidates 'agentic-code-helm-projectile--candidates
    :action '(("Send file reference(s) to Claude Code" . agentic-code-helm-projectile--send-file-references))
    :fuzzy-match t
    :filtered-candidate-transformer 'helm-fuzzy-highlight-matches)
  "Helm source for projectile files with send-to-Claude-Code action.")

;;;###autoload
(cl-defun agentic-code-helm-projectile-send-files ()
  "Select projectile files and send their references to Claude Code.
Mark multiple files with \\[helm-toggle-visible-mark] to send multiple references.
File references are formatted for Claude Code and sent to the active session.

Each file is sent as a separate @filename reference."
  (interactive)
  (cond
   ((not (featurep 'projectile))
    (user-error "Projectile is not available. Please install and require projectile"))
   ((not (projectile-project-p))
    (user-error "Not in a projectile project"))
   (t
    (helm :sources 'agentic-code-helm-projectile-source
          :buffer "*helm projectile copy files*"
          :truncate-lines t
          :prompt "Select file(s) to send to Claude Code: "))))

;;;; Footer

(provide 'agentic-code-helm)
;;; agentic-code-helm.el ends here
