
;;; agentic-code-transient.el --- Transient menu for agentic-code -*- lexical-binding: t; -*-

;; Copyright (C) 2025 imakado

;; Author: imakado <imakado@gmail.com>
;; Version: 0.1.0
;; Keywords: tools, convenience, ai
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

;; This package provides a transient menu interface for agentic-code.
;; It offers a convenient, magit-style menu for accessing the main
;; agentic-code operations.
;;
;; Usage:
;;   M-x agentic-code-transient-menu
;;
;; The menu provides the following operations:
;; - Start Claude Code session in project root
;; - Start Claude Code session in any directory
;; - Select active sessions with helm
;; - Send file reference to Claude Code
;; - Copy file reference to clipboard
;; - Open edit buffer for message composition
;; - Window layout management

;;; Code:

;;;; Dependencies

(require 'transient)
(require 'dash)
(require 's)
(require 'f)
(require 'projectile)

;;;; Forward declarations
(defvar agentic-code--sessions)
(declare-function agentic-code--get-project-root "agentic-code")
(declare-function agentic-code--session-find "agentic-code")
(declare-function agentic-code-start-claude-code-project-root "agentic-code")
(declare-function agentic-code-start-claude-code "agentic-code")
(declare-function agentic-code-helm-select-session "agentic-code-helm")
(declare-function agentic-code-send-claude-file-reference-dwim "agentic-code")
(declare-function agentic-code-claude-file-reference-string-to-clipboard "agentic-code")
(declare-function agentic-code-edit-in-buffer "agentic-code")
(declare-function agentic-code-layout-setup-2-columns "agentic-code-window-layout-config")
(declare-function agentic-code-layout-setup-3-columns "agentic-code-window-layout-config")
(declare-function agentic-code-layout-reset "agentic-code-window-layout-config")
(declare-function agentic-code-helm-projectile-send-files "agentic-code-helm")
(declare-function agentic-code-cycle-window-configurations "agentic-code")


;;;; Helper Functions

(defun agentic-code-transient--active-sessions-count ()
  "Return the count of active Claude Code sessions."
  (cond
   ((boundp 'agentic-code--sessions)
    (length (cl-loop for (_ . session-info) in agentic-code--sessions
                     when (let-alist session-info
                            (and .process
                                 (processp .process)
                                 (process-live-p .process)))
                     collect session-info)))
   (t 0)))

(defun agentic-code-transient--current-session-info ()
  "Get information about the current project's Claude Code session."
  (when (and (boundp 'agentic-code--sessions)
             (fboundp 'agentic-code--get-project-root))
    (ignore-errors
      (let* ((root-dir (agentic-code--get-project-root))
             (session-info (agentic-code--session-find agentic-code--sessions root-dir)))
        (when session-info
          (let-alist session-info
            (and .process
                 (processp .process)
                 (process-live-p .process)
                 session-info)))))))

(defun agentic-code-transient--format-session-status ()
  "Format the session status for display."
  (let* ((active-count (agentic-code-transient--active-sessions-count))
         (current-session (agentic-code-transient--current-session-info)))
    (concat
     (propertize "Claude Code Sessions" 'face 'transient-heading)
     (propertize " | " 'face 'transient-delimiter)
     (cond
      ((and current-session (> active-count 0))
       (let-alist current-session
         (format "%s (+ %d more)"
                 (propertize (f-filename .root-dir) 'face 'success)
                 (1- active-count))))
      ((> active-count 0)
       (format "%d active session%s"
               active-count
               (if (= active-count 1) "" "s")))
      (t
       (propertize "No active sessions" 'face 'transient-inactive-value))))))

(defun agentic-code-transient--session-suffix ()
  "Return a suffix indicator for session commands."
  (let* ((count (agentic-code-transient--active-sessions-count)))
    (cond
     ((> count 0)
      (propertize (format "[%d]" count) 'face 'transient-value))
     (t ""))))

(defun agentic-code-transient--current-file-suffix ()
  "Return a suffix showing current file info."
  (cond
   (buffer-file-name
    (propertize (format "[%s]" (f-filename buffer-file-name))
                'face 'transient-value))
   (t "")))

(defun agentic-code-transient--projectile-project-suffix ()
  "Return suffix showing current project name for projectile commands."
  (when (fboundp 'projectile-project-name)
    (ignore-errors
      (when-let ((name (projectile-project-name)))
        (propertize (format "[%s]" name)
                    'face 'transient-value)))))

;;;; Layout Transient

(transient-define-prefix agentic-code-transient-layout ()
  "Window layout management for Agentic Code."
  ["Layout Presets"
   ("2" "2-column layout" agentic-code-layout-setup-2-columns
    :description "Setup 2-column layout (context | terminal)")
   ("3" "3-column layout" agentic-code-layout-setup-3-columns
    :description "Setup 3-column layout (history | context | terminal)")]
  ["Layout Control"
   ("r" "Reset layout" agentic-code-layout-reset
    :description "Reset window layout to original state")]
  ["Navigation"
   ("q" "Back to main menu" transient-quit-one)])

;;;; Main Menu

;;;###autoload
(transient-define-prefix agentic-code-transient-menu ()
  "Transient menu for agentic-code operations."
  :man-page "agentic-code"
  [:description agentic-code-transient--format-session-status
                [""]]

  ["Session Management"
   :class transient-column
   :setup-children (lambda (_)
                     (transient-parse-suffixes
                      'agentic-code-transient-menu
                      (list (list "o" "Start in project root"
                                  'agentic-code-start-claude-code-project-root
                                  :description (lambda ()
                                                 (let* ((session (agentic-code-transient--current-session-info)))
                                                   (cond
                                                    (session "Restart in project root")
                                                    (t "Start in project root")))))
                            (list "d" "Start in directory..."
                                  'agentic-code-start-claude-code
                                  :description "Start Claude Code in selected directory")
                            (list "h" "Select session"
                                  'agentic-code-helm-select-session
                                  :description (lambda ()
                                                 (format "Select session %s"
                                                         (agentic-code-transient--session-suffix))))
                            (list "w" "Cycle window configs"
                                  'agentic-code-cycle-window-configurations
                                  :description (lambda ()
                                                 (let* ((count (agentic-code-transient--active-sessions-count)))
                                                   (cond
                                                    ((> count 0)
                                                     (format "Cycle window configs [%d]" count))
                                                    (t "Cycle window configs"))))))))]

  ["File & Editing"
   :class transient-column
   ("f" "Send file reference" agentic-code-send-claude-file-reference-dwim
    :description (lambda ()
                   (format "Send file reference %s"
                           (agentic-code-transient--current-file-suffix))))
   ("p" "Pick project files and send" agentic-code-helm-projectile-send-files
    :description (lambda ()
                   (format "Pick project files and send %s"
                           (or (agentic-code-transient--projectile-project-suffix) ""))))
   ("c" "Copy file reference" agentic-code-claude-file-reference-string-to-clipboard
    :description (lambda ()
                   (format "Copy file reference %s"
                           (agentic-code-transient--current-file-suffix))))
   ("e" "Edit in buffer" agentic-code-edit-in-buffer
    :description "Open edit buffer for message composition"
    :if (lambda () (eq major-mode 'vterm-mode)))]

  [["Help"
    :class transient-column
    ("?" "Show keybindings" (lambda ()
                              (interactive)
                              (describe-keymap 'agentic-code-command-map))
     :transient t)]
   ["Quit"
    :class transient-column
    ("q" "Quit" transient-quit-one)]
   ["Layout & UI"
    :class transient-column
    ("l" "Layout management" agentic-code-transient-layout
     :transient transient--do-replace
     :description "Window layout presets →")
    ("m" "Open this menu" agentic-code-transient-menu
     :description "Show this transient menu")]])

(provide 'agentic-code-transient)

;;; agentic-code-transient.el ends here
