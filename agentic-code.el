;;; agentic-code.el --- AI-powered code generation and editing -*- lexical-binding: t -*-

;; Copyright (C) 2025 imakado

;; Author: imakado <imakado@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (vterm "0.0.2") (dash "2.19.1") (s "1.13.0") (f "0.20.0") (markdown-mode "2.5") (helm "3.9.9") (window-layout "1.5") (transient "0.7.5") (compat "30.1") (projectile "2.8.0"))
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

;; Agentic Code provides AI-powered code generation and editing capabilities
;; directly within Emacs.  It integrates with the Claude Code CLI through
;; vterm.el to enable seamless interaction with AI agents for various
;; programming tasks.
;;
;; Features:
;; - Seamless integration with Claude Code CLI via vterm
;; - Project-aware session management
;; - File reference sending with region support
;; - Edit buffer for composing complex messages
;; - Window layout management for optimal workflow
;; - Transient menu interface for easy access to all features
;;
;; Quick Start:
;; 1. Install the Claude CLI (see https://claude.ai/code for instructions)
;; 2. Ensure vterm.el is installed and configured
;; 3. Add agentic-code to your load-path
;; 4. (require 'agentic-code)
;; 5. M-x agentic-code-transient-menu to open the main menu
;;
;; Basic Usage:
;; - Use the transient menu (M-x agentic-code-transient-menu) for all operations
;; - Start Claude Code in project root: o
;; - Send current file reference: f
;; - Open edit buffer for complex prompts: e
;;
;; Key Bindings:
;; You can bind the command map to a prefix key:
;; (global-set-key (kbd "C-c a") 'agentic-code-command-map)
;;
;; This provides quick access to:
;; C-c a o - Start Claude Code in project root
;; C-c a f - Send file reference
;; C-c a e - Edit in buffer
;; C-c a m - Open transient menu
;; And more...

;;; Code:

;;;; Dependencies

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'vterm)
(require 'project)
(require 'markdown-mode)
(require 'helm)
(require 'window-layout)
(require 'transient)
(require 'compat)
(require 'projectile)

(require 'agentic-code-helm)
(require 'agentic-code-log)
(require 'agentic-code-transient)
(require 'agentic-code-window-layout-config)

;;;; Customization

(defgroup agentic-code nil
  "AI-powered code generation and editing for Emacs."
  :group 'tools
  :prefix "agentic-code-")

(defcustom agentic-code-cli-command "claude"
  "Command to invoke the Claude CLI.

This should be either the full path to the claude executable
or just the command name if it is in your PATH.  The default value
assumes 'claude' is available in PATH."
  :type 'string
  :group 'agentic-code)

(defcustom agentic-code-edit-buffer-name-format "*agentic-code-edit [%s]*"
  "Name of the temporary buffer for editing messages."
  :type 'string
  :group 'agentic-code)

(defcustom agentic-code-claude-command-format "%s "
  "Format string for Claude Code CLI command execution.

This format string is used with the `agentic-code-cli-command' to
construct the full command line when starting a Claude Code session.
The %s placeholder will be replaced with the value of
`agentic-code-cli-command'.

For example, with the default format '%s ' and the default
CLI command 'claude', the resulting command will be 'claude '."
  :type 'string
  :group 'agentic-code)

(defcustom agentic-code-remove-claude-code-suggestion t
  "Whether to remove Claude Code's initial prompt suggestions.

When non-nil, removes suggestions like 'Try \"...\"' that appear
at the beginning of text when opening the edit buffer.  This is
useful for the current version of Claude Code, but may become
unnecessary in future updates."
  :type 'boolean
  :group 'agentic-code)

(defcustom agentic-code-show-project-root-in-header t
  "Whether to show the project root directory in vterm buffer header.

When non-nil, displays the project root directory in the header
line of vterm buffers running Claude Code sessions.  The path is
displayed in abbreviated form using `abbreviate-file-name'."
  :type 'boolean
  :group 'agentic-code)

(defface agentic-code-header-project-label-face
  '((t :inherit font-lock-comment-face
       :weight bold
       :height 1.1))
  "Face for the 'Project:' label in vterm header lines.

This face is used for the label text that appears before the
project path in the header line.  By default, it uses a bold
weight and slightly larger font size."
  :group 'agentic-code)

(defface agentic-code-header-project-path-face
  '((t :inherit font-lock-string-face
       :height 1.1))
  "Face for the project path in vterm header lines.

This face is used for displaying the actual project directory
path in the header line.  By default, it uses a slightly larger
font size with coloring from the string face."
  :group 'agentic-code)


;;;; Variables

(defvar-local agentic-code--source-vterm-buffer nil
  "The vterm buffer from which the edit command was invoked.

This buffer-local variable stores a reference to the original vterm
buffer to ensure the edited content is sent back to the correct
session.")
(put 'agentic-code--source-vterm-buffer 'permanent-local t)

(defvar-local agentic-code--current-edit-buffer nil
  "The edit buffer currently associated with this vterm session.

This buffer-local variable stores a reference to the temporary edit
buffer created from this vterm session, allowing tracking of active
edit operations.")

(defvar-local agentic-code--claude-root-directory nil
  "")
(put 'agentic-code--claude-root-directory 'permanent-local t)

(defvar agentic-code--sessions nil
  "Global registry of active Claude Code sessions.

This variable maintains a list of association lists, where each
session is represented as:

  (PROJECT-ROOT . SESSION-INFO)

Where PROJECT-ROOT is a string path returned by `agentic-code--get-project-root'
and SESSION-INFO is an association list with keys:

  'buffer-name   - Name of the vterm buffer (string)
  'vterm-buffer  - The actual vterm buffer object
  'process       - The vterm process object
  'root-dir      - Directory where session was started (string)
  'last-displayed - Unix timestamp when buffer was last displayed (integer)
  'created-at    - Unix timestamp when session was created (integer)
  'window-config - Current window configuration (window-configuration object)

Example structure:
  '((\"/project/root\" . ((buffer-name . \"*claude-code project*\")
                         (vterm-buffer . #<buffer>)
                         (process . #<process>)
                         (root-dir . \"/project/root\")
                         (last-displayed . 1735123456)
                         (created-at . 1735123456)
                         (window-config . #<window-configuration>))))")

(defvar agentic-code-claude-vterm-buffer-name-regexp
  "^\\*claude-code.*\\*"
  "")

(defvar agentic-code-edit-buffer-name-regexp
  "^\\*agentic-code-edit.*\\*")

(defvar agentic-code-claude-code-initial-suggestion-regexp
  (rx bol "Try \"" (1+ (not "\"")) "\"")
  "Regexp to match Claude Code's initial suggestion prompt.")

(defvar agentic-code--window-config-cycle-index nil
  "Current index for cycling through window configurations.

This variable tracks the current position when cycling through
session window configurations with `agentic-code-cycle-window-configurations'.
It is reset to nil when a different command is executed.")

(defvar agentic-code-window-config-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" 'agentic-code-cycle-window-configurations)
    map)
  "Repeat map for window configuration cycling.

When `repeat-mode' is enabled, allows pressing 'w' repeatedly
to cycle through window configurations without the prefix key.")

;;;; Core Functions

(defun agentic-code--ensure-dependencies ()
  "Ensure all required dependencies are available.

This function checks that vterm is loaded and the Claude Code CLI
is available in the system PATH.  Signals an error if any dependency
is missing."
  (cond
   ((not (fboundp 'vterm))
    (error "vterm.el is required but not available"))
   ((not (executable-find agentic-code-cli-command))
    (error "claude command not found in PATH"))
   (t t)))

(cl-defun agentic-code--get-project-root ()
  "Get the root directory of the current project.

Tries to determine the project root using projectile or built-in
project.el.  Signals an error if no project root can be found.

Returns the absolute path to the project root directory."
  (let* ((root-dir
          (cond
           ((fboundp 'projectile-project-root)
            (projectile-project-root))
           ((require 'project nil t)
            (project-root (project-current t)))
           (t
            (user-error "project root not found")))))

    (when (called-interactively-p 'interactive)
      (message "project root detected: %s" root-dir))
    root-dir))

;;;; Session Management Functions

(cl-defun agentic-code--session-find (sessions root-dir)
  "Find session information for ROOT-DIR in SESSIONS list.

SESSIONS is a list of session association lists.
ROOT-DIR is the project root directory path to search for.

Returns the session information association list if found, nil otherwise."
  (let* ((session-entry (assoc root-dir sessions)))
    (cond
     (session-entry (cdr session-entry))
     (t nil))))

(cl-defun agentic-code--session-add (sessions root-dir session-info)
  "Add or update session information in SESSIONS list.

SESSIONS is a list of session association lists.
ROOT-DIR is the project root directory path.
SESSION-INFO is an association list containing session data.

Returns a new sessions list with the session added or updated.
This function is pure and does not modify the original SESSIONS list."
  (let* ((existing-entry (assoc root-dir sessions))
         (other-sessions (cl-loop for session in sessions
                                  when (not (string= (car session) root-dir))
                                  collect session)))
    (cons (cons root-dir session-info) other-sessions)))

(cl-defun agentic-code--session-update (sessions root-dir key value)
  "Update a specific KEY with VALUE in session for ROOT-DIR.

SESSIONS is a list of session association lists.
ROOT-DIR is the project root directory path.
KEY is the session property to update.
VALUE is the new value for the property.

Returns a new sessions list with the session property updated.
If the session does not exist, returns the original SESSIONS list unchanged."
  (let* ((session-info (agentic-code--session-find sessions root-dir)))
    (cond
     (session-info
      (let* ((updated-info (cons (cons key value)
                                 (cl-loop for (k . v) in session-info
                                          when (not (eq k key))
                                          collect (cons k v)))))
        (agentic-code--session-add sessions root-dir updated-info)))
     (t sessions))))

(cl-defun agentic-code--session-remove (sessions root-dir)
  "Remove session for ROOT-DIR from SESSIONS list.

SESSIONS is a list of session association lists.
ROOT-DIR is the project root directory path to remove.

Returns a new sessions list with the session removed.
This function is pure and does not modify the original SESSIONS list."
  (cl-loop for session in sessions
           when (not (string= (car session) root-dir))
           collect session))

(cl-defun agentic-code--session-filter-alive (sessions)
  "Filter SESSIONS to only include sessions with live processes.

SESSIONS is a list of session association lists.

Returns a new sessions list containing only sessions where the
vterm process is still alive. This function is pure and does
not modify the original SESSIONS list."
  (cl-loop for (root-dir . session-info) in sessions
           when (let-alist session-info
                  (and .process
                       (processp .process)
                       (process-live-p .process)))
           collect (cons root-dir session-info)))

(cl-defun agentic-code--current-timestamp ()
  "Get current Unix timestamp as integer.

Returns the current time as Unix timestamp (seconds since epoch)
for use in session management."
  (time-to-seconds (current-time)))

(cl-defun agentic-code--session-save-window-config! (root-dir)
  "Save current window configuration for session with side effects.

ROOT-DIR is the project root directory path for the session to update.

This function saves the current window configuration in the global
`agentic-code--sessions' variable. It has side effects."
  (let* ((window-config (current-window-configuration))
         (current-time (agentic-code--current-timestamp))
         (session-info (agentic-code--session-find agentic-code--sessions root-dir)))
    (when session-info
      (let* ((updated-info (cons (cons 'window-config window-config)
                                 (cons (cons 'last-displayed current-time)
                                       (cl-loop for (k . v) in session-info
                                                when (not (member k '(window-config last-displayed)))
                                                collect (cons k v))))))
        (setq agentic-code--sessions
              (agentic-code--session-add agentic-code--sessions root-dir updated-info))))))

(cl-defun agentic-code--session-register! (root-dir vterm-buffer vterm-process)
  "Register a new session with side effects on global state.

ROOT-DIR is the project root directory path.
VTERM-BUFFER is the vterm buffer object.
VTERM-PROCESS is the vterm process object.

This function creates session information and updates the global
`agentic-code--sessions' variable. It has side effects."
  (let* ((current-time (agentic-code--current-timestamp))
         (buffer-name (buffer-name vterm-buffer))
         (window-config (current-window-configuration))
         (session-info `((buffer-name . ,buffer-name)
                         (vterm-buffer . ,vterm-buffer)
                         (process . ,vterm-process)
                         (root-dir . ,root-dir)
                         (last-displayed . ,current-time)
                         (created-at . ,current-time)
                         (window-config . ,window-config))))
    (setq agentic-code--sessions
          (agentic-code--session-add agentic-code--sessions root-dir session-info))))

(cl-defun agentic-code--session-update-timestamp! (root-dir)
  "Update the last-displayed timestamp for session with side effects.

ROOT-DIR is the project root directory path for the session to update.

This function updates the 'last-displayed timestamp in the global
`agentic-code--sessions' variable. It has side effects."
  (let* ((current-time (agentic-code--current-timestamp)))
    (setq agentic-code--sessions
          (agentic-code--session-update agentic-code--sessions
                                        root-dir
                                        'last-displayed
                                        current-time))))

(cl-defun agentic-code--session-cleanup-dead! ()
  "Remove sessions with dead processes with side effects on global state.

This function filters out sessions where the vterm process is no longer
alive and updates the global `agentic-code--sessions' variable.
It has side effects."
  (setq agentic-code--sessions
        (agentic-code--session-filter-alive agentic-code--sessions)))

(cl-defun agentic-code-make-vterm-buffer-name (target-dir &optional root-dir)
  "Generate a buffer name for the vterm session."
  (let* ((name
          (cond
           (root-dir
            (f-relative target-dir
                        (f-parent root-dir)))
           (t
            (let* ((dirname (f-filename target-dir)))
              dirname)))))
    (format "*claude-code %s*" name)))

(cl-defun agentic-code--get-claude-file-reference-string (&key (region-support-p t)
                                                               (file-path nil))
  "Generate a Claude Code file reference string for the current buffer location."
  (let* ((path (or file-path
                   (buffer-file-name)))
         (file-rel-path
          (f-relative path (agentic-code--get-project-root))))

    (cond
     ((and (use-region-p)
           region-support-p)
      (let* ((start-pos (region-beginning))
             (end-pos (region-end))
             (start-line (line-number-at-pos start-pos))
             (end-line (line-number-at-pos end-pos)))
        (format " @%s#L%d-%d " file-rel-path start-line end-line)))
     (t
      (format " @%s " file-rel-path)))))


;;;; Interactive Commands

;;;###autoload
(cl-defun agentic-code-claude-file-reference-string-to-clipboard ()
  "Copy a Claude Code file reference string to the clipboard."
  (interactive)
  (--> (agentic-code--get-claude-file-reference-string)
       (kill-new it)))

;;;###autoload
(cl-defun agentic-code-cycle-window-configurations ()
  "Cycle through saved window configurations of all sessions.

When called repeatedly, cycles through window configurations
from all sessions ordered by their last-displayed timestamp
(most recent first).

After cycling through all sessions, returns to the most recent one.
This creates a circular navigation through all saved configurations."
  (interactive)
  ;; Clean up dead sessions first
  (agentic-code--session-cleanup-dead!)
  
  (let* ((sessions-with-config
          (cl-loop for (root-dir . session-info) in agentic-code--sessions
                   when (let-alist session-info
                          (and .window-config
                               .last-displayed))
                   collect (cons root-dir session-info)))
         (sorted-sessions
          (cl-sort sessions-with-config
                   (lambda (a b)
                     (> (alist-get 'last-displayed (cdr a))
                        (alist-get 'last-displayed (cdr b))))))
         (session-count (length sorted-sessions)))
    
    (cond
     ;; No sessions with window configurations
     ((zerop session-count)
      (message "No saved window configurations found"))
     
     ;; Check if this is a consecutive call
     ((and (eq this-command real-last-command)
           agentic-code--window-config-cycle-index)
      ;; Increment index and wrap around
      (setq agentic-code--window-config-cycle-index
            (mod (1+ agentic-code--window-config-cycle-index) session-count))
      
      ;; Restore the window configuration at current index
      (let* ((session-entry (nth agentic-code--window-config-cycle-index sorted-sessions))
             (root-dir (car session-entry))
             (session-info (cdr session-entry)))
        (let-alist session-info
          (set-window-configuration .window-config)
          (message "Restored window configuration [%d/%d]: %s"
                   (1+ agentic-code--window-config-cycle-index)
                   session-count
                   (abbreviate-file-name root-dir)))))
     
     ;; First call or different command
     (t
      ;; Start from the beginning (most recent)
      (setq agentic-code--window-config-cycle-index 0)
      
      (let* ((session-entry (car sorted-sessions))
             (root-dir (car session-entry))
             (session-info (cdr session-entry)))
        (let-alist session-info
          (set-window-configuration .window-config)
          (message "Restored window configuration [1/%d]: %s"
                   session-count
                   (abbreviate-file-name root-dir))))))))

;; Setup repeat-mode support for the cycle command
(put 'agentic-code-cycle-window-configurations 'repeat-map 'agentic-code-window-config-repeat-map)

(cl-defun agentic-code--send-claude-file-reference-dwim-reference-string (str)
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at (rx bol (* space) eol)))
         (delete-region (line-beginning-position)
                        (line-end-position))
         (s-trim-left str))
        (t (format "%s" str))))

;;;###autoload
(cl-defun agentic-code-send-claude-file-reference-dwim (&key (file-reference-string nil))
  "Send the current file reference to Claude Code with edit buffer.

If Claude Code is not running, saves the reference to kill-ring
and prompts the user to start Claude Code manually."
  (interactive)
  (let* ((reference-string
          (or file-reference-string
              (agentic-code--get-claude-file-reference-string)))
         (root-dir (agentic-code--get-project-root))
         (vterm-buffer-name (agentic-code-make-vterm-buffer-name root-dir)))
    ;; Start or switch to Claude Code session
    (agentic-code-start-claude-code-project-root)

    (let* ((vterm-buffer (get-buffer vterm-buffer-name)))
      (cond
       ;; When vterm buffer is not found
       ((not vterm-buffer)
        (kill-new reference-string)
        (error "Failed to create vterm buffer. Reference saved to kill-ring"))

       ;; When vterm buffer exists, check its status
       (t
        (cl-case (agentic-code--check-claude-code-status vterm-buffer)
          ;; Claude Code is running properly
          (claude-code-ready
           (let* ((existing-edit-buffer (buffer-local-value 'agentic-code--current-edit-buffer vterm-buffer)))
             (cond
              ;; If edit buffer exists and is alive, append to it
              ((and existing-edit-buffer (buffer-live-p existing-edit-buffer))
               (display-buffer existing-edit-buffer)
               (select-window (get-buffer-window existing-edit-buffer))
               (with-current-buffer existing-edit-buffer
                 (goto-char (point-max))
                 (let* ((reference-string-trim
                         (agentic-code--send-claude-file-reference-dwim-reference-string reference-string)))
                   (insert reference-string-trim))))
              ;; Otherwise, create new edit buffer
              (t
               (agentic-code-edit-in-buffer)
               (insert (agentic-code--send-claude-file-reference-dwim-reference-string
                        reference-string))))))

          ;; When at shell prompt
          (shell-prompt
           (kill-new reference-string)
           (message (concat "Claude Code is not running. "
                            "Reference saved to kill-ring. "
                            "Please start Claude Code in the terminal buffer with: %s")
                    (format agentic-code-claude-command-format agentic-code-cli-command)))

          ;; When status is unknown
          (unknown
           (kill-new reference-string)
           (message (concat "Cannot determine Claude Code status. "
                            "Reference saved to kill-ring. "
                            "Please check the terminal buffer and start Claude Code if needed.")))))))))



;;;###autoload
(defun agentic-code-edit-in-buffer ()
  "Open a temporary buffer to compose a message for the vterm session.

This command creates a new temporary buffer where you can freely
compose text.  When finished, press C-c C-c to send the buffer
contents to the vterm session from which this command was invoked.

The temporary buffer is automatically closed after sending.  If the
original vterm buffer has been deleted, the content is copied to
the clipboard and a warning message is displayed."
  (interactive)
  (agentic-code--ensure-dependencies)
  (let* ((vterm-source-buffer (current-buffer))
         (current-input-text (agentic-code--edit-take-current-input-text))
         (temp-edit-buffer-name (generate-new-buffer-name
                                 (format agentic-code-edit-buffer-name-format
                                         (buffer-name vterm-source-buffer))))
         (temp-edit-buffer (get-buffer-create temp-edit-buffer-name)))
    (cond
     ((not (eq major-mode 'vterm-mode))
      (user-error "This command must be run from a vterm buffer"))
     (t
      (with-current-buffer temp-edit-buffer
        (agentic-code-edit-mode)
        (setq-local agentic-code--source-vterm-buffer vterm-source-buffer))

      ;; Set reference to edit buffer in vterm buffer
      (with-current-buffer vterm-source-buffer
        (setq-local agentic-code--current-edit-buffer temp-edit-buffer))

      (display-buffer temp-edit-buffer)
      (select-window (get-buffer-window temp-edit-buffer))

      ;; Save window configuration after displaying edit buffer
      (let* ((project-root (with-current-buffer vterm-source-buffer
                             agentic-code--claude-root-directory)))
        (when project-root
          (agentic-code--session-save-window-config! project-root)))

      (when (and current-input-text (not (string-empty-p current-input-text)))
        (insert (cond
                 (agentic-code-remove-claude-code-suggestion
                  (replace-regexp-in-string
                   agentic-code-claude-code-initial-suggestion-regexp
                   ""
                   current-input-text))
                 (t current-input-text))))
      (message "Type your message. Press C-c C-c to send, C-c C-k to cancel")))))


(cl-defun agentic-code--vterm-wait-until (&key predicate (max-retries 15) (wait-interval 0.1))
  "Wait until PREDICATE returns non-nil in vterm context.

PREDICATE is a function with no arguments that returns non-nil when
the desired vterm state is reached.
MAX-RETRIES is the maximum number of attempts (default: 15).
WAIT-INTERVAL is the time to wait between attempts in seconds (default: 0.1).

Returns t if PREDICATE returned non-nil within MAX-RETRIES attempts,
nil otherwise."
  (cl-loop for retry-count from 1 to max-retries
           do (cond
               ((funcall predicate)
                (cl-return t))
               (t
                (sit-for wait-interval)))
           finally return nil))



(cl-defun agentic-code--edit-send-and-close-cleanup (&key edit-buffer source-buffer)
  ""
  (delete-window (get-buffer-window edit-buffer))
  (kill-buffer edit-buffer))

(defun agentic-code-edit-send-and-close ()
  "Send the buffer contents to the source vterm and close this buffer.

This function sends the entire contents of the current buffer to
the vterm session stored in `agentic-code--source-vterm-buffer'.
If the source buffer no longer exists, the content is copied to
the clipboard instead."
  (interactive)
  (let* ((content (buffer-string))
         (source-buffer agentic-code--source-vterm-buffer)
         (max-retries 10)
         (wait-interval 0.1)
         (cleared-p nil)
         (source-buffer-window (get-buffer-window source-buffer))
         (edit-buffer (current-buffer)))
    (cond
     ((and source-buffer
           (buffer-live-p source-buffer)
           (process-live-p (get-buffer-process source-buffer)))
      (with-current-buffer source-buffer
        (vterm-clear)
        ;; Wait for vterm-clear to take effect
        (setq cleared-p (agentic-code--vterm-wait-until
                         :predicate (lambda ()
                                      (let* ((current-input (agentic-code--edit-take-current-input-text)))
                                        (or (null current-input)
                                            (string-empty-p current-input)
                                            ;; hmm...
                                            (string-match agentic-code-claude-code-initial-suggestion-regexp
                                                          current-input))))
                         :max-retries max-retries
                         :wait-interval wait-interval)))
      (cond
       (cleared-p
        (with-current-buffer source-buffer
          (vterm-send-string content))
        ;; (delete-window source-buffer-window)
        ;; (kill-buffer edit-buffer)
        (agentic-code--edit-send-and-close-cleanup
         :edit-buffer edit-buffer
         :source-buffer source-buffer)
        ;; (switch-to-buffer source-buffer)
        (select-window source-buffer-window)
        ;; Save window configuration after closing edit buffer
        (let* ((project-root (with-current-buffer source-buffer
                               agentic-code--claude-root-directory)))
          (when project-root
            (agentic-code--session-save-window-config! project-root)))
        (message "Message sent to vterm"))
       (t
        ;; If clearing fails, copy to clipboard and warn
        (kill-new content)
        (agentic-code--edit-send-and-close-cleanup
         :edit-buffer edit-buffer
         :source-buffer source-buffer)
        (select-window source-buffer)
        ;; Save window configuration after closing edit buffer
        (let* ((project-root (with-current-buffer source-buffer
                               agentic-code--claude-root-directory)))
          (when project-root
            (agentic-code--session-save-window-config! project-root)))
        (message "Failed to clear vterm input area after %d retries. Content copied to clipboard - please paste manually with C-y" max-retries))))
     (t
      (kill-new content)
      (agentic-code--edit-send-and-close-cleanup
       :edit-buffer edit-buffer
       :source-buffer source-buffer)
      (message "Source vterm buffer was deleted. Content copied to clipboard")))))

(defun agentic-code-edit-cancel ()
  "Cancel the edit and close the buffer without sending.

This function closes the temporary edit buffer without sending
its contents to the vterm session."
  (interactive)
  (let* ((source-buffer agentic-code--source-vterm-buffer)
         (cur-buf (current-buffer)))
    ;; (delete-window (get-buffer-window source-buffer))
    (delete-window (get-buffer-window cur-buf))
    (kill-buffer cur-buf)
    (when (and source-buffer (buffer-live-p source-buffer))
      (select-window (get-buffer-window source-buffer))
      ;; Save window configuration after closing edit buffer
      (let* ((project-root (with-current-buffer source-buffer
                             agentic-code--claude-root-directory)))
        (when project-root
          (agentic-code--session-save-window-config! project-root))))
    (message "Edit cancelled")))


(cl-defun agentic-code-start-claude-code-project-root (&key (root-dir (agentic-code--get-project-root)))
  "Start a Claude Code session in a vterm buffer.

ROOT-DIR is the directory where the Claude Code command will be
executed.  Defaults to the current project root.

This command creates a new vterm buffer or switches to an existing
one, then starts the Claude Code CLI with appropriate options."
  (interactive)
  (agentic-code--ensure-dependencies)

  (let* ((vterm-buffer-name (agentic-code-make-vterm-buffer-name root-dir))
         (existing-buffer (get-buffer vterm-buffer-name)))
    (cond
     ;; If buffer already exists, just display it and update timestamp
     (existing-buffer
      (display-buffer existing-buffer)
      (select-window (get-buffer-window existing-buffer))
      (with-current-buffer existing-buffer
        (setq-local agentic-code--claude-root-directory root-dir))
      (agentic-code--session-save-window-config! root-dir))
     ;; Otherwise, create new vterm buffer and start Claude Code
     (t
      (let* ((default-directory root-dir))
        (vterm vterm-buffer-name)
        (let* ((vterm-buf (get-buffer vterm-buffer-name))
               (vterm-process (get-buffer-process vterm-buf)))
          (with-current-buffer vterm-buf
            (agentic-code-vterm-minor-mode t)
            (setq-local agentic-code--claude-root-directory root-dir)
            ;; TODO: make CLI options customizable
            (vterm-send-string (format agentic-code-claude-command-format agentic-code-cli-command))
            ;; (vterm-send-return)
            )
          ;; Register the new session
          (agentic-code--session-register! root-dir vterm-buf vterm-process)))))))

;;;###autoload
(cl-defun agentic-code-start-claude-code ()
  "Start Claude Code in a user-selected directory.

This command prompts for a directory and starts a Claude Code session
in that location.  Unlike `agentic-code-start-claude-code-project-root',
this allows starting Claude Code in any directory, not just project roots."
  (interactive)
  (let* ((target-dir (read-directory-name "Start Claude Code in directory: " default-directory nil t)))
    (agentic-code-start-claude-code-project-root :root-dir target-dir)))

(cl-defun agentic-code--check-claude-code-status (vterm-buffer)
  "Check the status of Claude Code in VTERM-BUFFER.

Returns one of the following symbols:
- 'claude-code-ready : Claude Code is running and ready
- 'shell-prompt : Shell prompt detected (Claude Code not running)
- 'unknown : Cannot determine status"
  (with-current-buffer vterm-buffer
    (save-excursion
      (goto-char (point-max))
      (cond
       ;; Detect shell prompt
       ((save-excursion
          ;; Skip trailing whitespace
          (skip-chars-backward " \t\n\r")
          (end-of-line)
          (let* ((search-limit (max (line-beginning-position) (point-min))))
            ;; Look for '>' prompt in the last non-whitespace line
            (re-search-backward "^> " search-limit t)))
        'shell-prompt)
       ;; Look for Claude Code prompt box (priority)
       ((save-excursion
          ;; Detect box-drawing characters (within 5000 characters)
          (let* ((start (save-excursion
                          (re-search-backward "^╭" (max (- (point-max) 50000) (point-min)) t))))
            (and start
                 (save-excursion
                   (goto-char start)
                   (re-search-forward (rx bol "│ >") nil t)))))
        'claude-code-ready)
       ;; Status is unknown
       (t 'unknown)))))

(defun agentic-code--send-string (str)
  "Send STR to the active Claude Code vterm session.

STR is the string to send to the Claude Code process.  This function
ensures dependencies are available and sends the string to the
appropriate vterm buffer.

Signals an error if no active session is found."
  (agentic-code--ensure-dependencies)
  (let* ((vterm-buffer nil)) ;; TODO: implement logic to get the appropriate vterm buffer
    (when vterm-buffer
      (with-current-buffer vterm-buffer
        (vterm-send-string str)))))


;;;; Key bindings

;; Define layout submenu
(defvar agentic-code-layout-map
  (let ((map (make-sparse-keymap)))
    (define-key map "2" 'agentic-code-layout-setup-2-columns)
    (define-key map "3" 'agentic-code-layout-setup-3-columns)
    (define-key map "r" 'agentic-code-layout-reset)
    map)
  "Keymap for Agentic Code layout commands.")

;;;###autoload (autoload 'agentic-code-command-map "agentic-code")
(defvar agentic-code-command-map
  (let ((map (make-sparse-keymap)))
    ;; Session management
    (define-key map "o" 'agentic-code-start-claude-code-project-root)
    (define-key map "d" 'agentic-code-start-claude-code)
    (define-key map "h" 'agentic-code-helm-select-session)
    (define-key map "w" 'agentic-code-cycle-window-configurations)

    ;; File operations
    (define-key map "f" 'agentic-code-send-claude-file-reference-dwim)
    (define-key map "c" 'agentic-code-claude-file-reference-string-to-clipboard)

    ;; Editing
    (define-key map "e" 'agentic-code-edit-in-buffer)

    ;; UI and menus
    (define-key map "m" 'agentic-code-transient-menu)
    (define-key map "l" agentic-code-layout-map)

    map)
  "Keymap for Agentic Code commands.

This keymap is intended to be bound to a prefix key and provides
quick access to various Agentic Code functions.

Key bindings:
  o - Start Claude Code in project root
  d - Start Claude Code in selected directory
  h - Select session with helm
  w - Cycle through window configurations
  f - Send file reference to Claude Code
  c - Copy file reference to clipboard
  e - Edit message in buffer
  m - Open transient menu
  l - Layout commands prefix:
    l 2 - Setup 2-column layout
    l 3 - Setup 3-column layout
    l r - Reset layout")

;;;; Minor Mode

;;;###autoload
(define-minor-mode agentic-code-mode
  "Minor mode for AI-powered code assistance.

When enabled, this mode provides key bindings and functionality for
interacting with AI agents to assist with code generation, editing,
and other programming tasks.

\\{agentic-code-mode-map}"
  :lighter " AgenticCode"
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "C-c a s") #'agentic-code-start-session)
            ;; (define-key map (kbd "C-c a p") #'agentic-code-process-comment)
            map))

;;;; Edit Mode

(cl-defun agentic-code--edit-take-current-input-text ()
  (let* ((start (save-excursion
                  (goto-char (point-max))
                  (re-search-backward (rx "╭─") nil t)
                  (point)))
         (end (save-excursion
                (goto-char (point-max))
                (re-search-backward (rx "─╯") nil t)
                (line-end-position)))
         (input-string (or (buffer-substring-no-properties start end)
                           "")))
    (agentic-code--extract-boxed-content input-string)))

(defun agentic-code--extract-boxed-content (boxed-string)
  "Extract content from a Unicode box-drawn string.
The function removes the box borders, handles the '>' marker on the first line,
and adjusts indentation based on the first line's content position."
  (let* ((lines (s-lines boxed-string))
         ;; Filter out lines that are box borders (start with ╭, ╰, or contain only ─)
         (content-lines
          (cl-loop for line in lines
                   when (and (not (s-matches? "^╭" line))
                             (not (s-matches? "^╰" line))
                             (s-matches? ".*│$" line))
                   collect line))
         ;; Process first line to find base indentation
         (first-line (car content-lines))
         (base-indent
          (cond
           ((null first-line) 0)
           (t
            ;; Remove box borders and find position after '>'
            (let* ((trimmed (s-chop-prefix "│" (s-chop-suffix "│" first-line)))
                   ;; Find the '>' marker
                   (marker-pos (s-index-of ">" trimmed)))
              (cond
               ((null marker-pos) 0)
               ;; Base indent is position after '>' plus one space
               (t (+ marker-pos 2)))))))
         ;; Process all lines
         (processed-lines
          (cl-loop for line in content-lines
                   for index from 0
                   collect
                   (let* ((trimmed (s-chop-prefix "│" (s-chop-suffix "│" line)))
                          (dedented (substring-no-properties trimmed 3)))
                     ;; Trim trailing whitespace
                     (s-trim-right dedented)))))
    ;; Join lines with newlines
    (s-join "\n" processed-lines)))


(defvar agentic-code-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agentic-code-edit-send-and-close)
    (define-key map (kbd "C-c C-k") #'agentic-code-edit-cancel)
    map)
  "Keymap for `agentic-code-edit-mode'.

This keymap provides key bindings for the temporary edit buffer:
- C-c C-c: Send content and close buffer
- C-c C-k: Cancel and close buffer")

(define-derived-mode agentic-code-edit-mode markdown-mode "AgenticCode-Edit"
  "Major mode for editing messages to send to Claude Code.

This mode is derived from `markdown-mode' and is used in temporary
buffers created by `agentic-code-edit-in-buffer'.  It provides
Markdown syntax highlighting and key bindings for sending or
cancelling the edit operation.

\\{agentic-code-edit-mode-map}"
  (setq-local header-line-format
              '(:eval (format "Editing message for: %s (C-c C-c to send, C-c C-k to cancel)"
                              (buffer-name agentic-code--source-vterm-buffer)))))

;;;; Vterm Helper Functions

(defun agentic-code--format-vterm-header-line ()
  "Format the header line for vterm buffers.

Returns a formatted string showing the project root directory
if `agentic-code-show-project-root-in-header' is non-nil,
otherwise returns nil."
  (when (and agentic-code-show-project-root-in-header
             agentic-code--claude-root-directory)
    (concat " "
            (propertize "Project: "
                        'face 'agentic-code-header-project-label-face)
            (propertize (abbreviate-file-name agentic-code--claude-root-directory)
                        'face 'agentic-code-header-project-path-face)
            " ")))

;;;; Vterm Minor mode

(defvar agentic-code-vterm-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add agentic-code specific key bindings here
    (define-key map (kbd "C-c C-e") #'agentic-code-edit-in-buffer)
    map)
  "Keymap for `agentic-code-vterm-minor-mode'.

This keymap contains key bindings specific to the Agentic Code
vterm interface.  It provides additional bindings for AI agent
interaction while in vterm buffers.")

(define-minor-mode agentic-code-vterm-minor-mode
  "Minor mode for Agentic Code functionality in vterm buffers.

This mode provides additional key bindings and functionality
specific to AI agent interaction when working in vterm buffers
with Claude Code sessions.

\\{agentic-code-vterm-minor-mode-map}"
  :lighter " AC-vterm"
  :keymap agentic-code-vterm-minor-mode-map
  (cond
   (agentic-code-vterm-minor-mode
    ;; Enable mode specific behavior
    (setq-local header-line-format
                '(:eval (agentic-code--format-vterm-header-line))))
   (t
    ;; Disable mode specific behavior
    (setq-local header-line-format nil))))


;;;; Footer

(provide 'agentic-code)

;;; agentic-code.el ends here
