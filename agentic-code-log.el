;;; agentic-code-log.el --- Logging facility for agentic-code -*- lexical-binding: t; -*-

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

;; This package provides a comprehensive logging facility for agentic-code.
;; It supports multiple log levels (OFF, DEBUG, INFO, WARN, ERROR, FATAL)
;; and outputs to a dedicated buffer with timestamp and colorization.
;;
;; Features:
;; - Six log levels: OFF (-1), DEBUG (0), INFO (1), WARN (2), ERROR (3), FATAL (4)
;; - Customizable log level via `agentic-code-log-level'
;; - Dedicated log buffer with colorized output
;; - Timestamp formatting support
;; - Easy integration with agentic-code main package
;;
;; Usage:
;;   (require 'agentic-code-log)
;;   (setq agentic-code-log-level 'info) ; Enable INFO level and above
;;   (agentic-code-log-info "Starting process")
;;   (agentic-code-log-error "Failed to connect: %s" error-message)
;;
;; By default, logging is disabled (OFF level) to avoid unnecessary overhead.

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup agentic-code-log nil
  "Logging facility for agentic-code."
  :group 'agentic-code
  :prefix "agentic-code-log-")

(defcustom agentic-code-log-level nil
  "Current logging level.
nil = OFF (no logging)
='debug = DEBUG
='info = INFO
='warn = WARN
='error = ERROR
='fatal = FATAL
Only messages at or above this level will be logged.
Any false value (nil, ='false, etc.) is treated as OFF."
  :type '(choice (const :tag "OFF" nil)
                 (const :tag "DEBUG" debug)
                 (const :tag "INFO" info)
                 (const :tag "WARN" warn)
                 (const :tag "ERROR" error)
                 (const :tag "FATAL" fatal))
  :group 'agentic-code-log)

(defcustom agentic-code-log-buffer-name "*agentic-code-log*"
  "Name of the buffer where log messages are written."
  :type 'string
  :group 'agentic-code-log)

(defcustom agentic-code-log-time-format "%Y-%m-%d %H:%M:%S"
  "Format string for timestamps in log messages."
  :type 'string
  :group 'agentic-code-log)

;;;; Constants

(defconst agentic-code-log--level-names
  '((-1 . "OFF")
    (0 . "DEBUG")
    (1 . "INFO")
    (2 . "WARN")
    (3 . "ERROR")
    (4 . "FATAL"))
  "Mapping of log level numbers to names.")

(defconst agentic-code-log--level-faces
  '((0 . font-lock-comment-face)       ; DEBUG - gray
    (1 . font-lock-doc-face)           ; INFO - default
    (2 . warning)                      ; WARN - orange
    (3 . error)                        ; ERROR - red
    (4 . (:foreground "white" :background "red"))) ; FATAL - white on red
  "Mapping of log levels to faces for colorization.")

;;;; Internal Functions

(defun agentic-code-log--level-to-number (level)
  "Convert symbolic LEVEL to numeric value.
Returns -1 for any false value (nil, \'false, etc.)."
  (cond
   ((null level) -1)      ; nil = OFF
   ((eq level 'debug) 0)
   ((eq level 'info) 1)
   ((eq level 'warn) 2)
   ((eq level 'error) 3)
   ((eq level 'fatal) 4)
   ((not level) -1)       ; Any other false value = OFF
   (t -1)))               ; Unknown = OFF

(defun agentic-code-log--get-level-name (level)
  "Get the name for numeric LEVEL."
  (or (alist-get level agentic-code-log--level-names)
      "UNKNOWN"))

(defun agentic-code-log--get-buffer ()
  "Get or create the log buffer."
  (get-buffer-create agentic-code-log-buffer-name))

(defun agentic-code-log--format-message (level format-string &rest args)
  "Format a log message with LEVEL, FORMAT-STRING and ARGS."
  (let* ((timestamp (format-time-string agentic-code-log-time-format))
         (level-name (agentic-code-log--get-level-name level))
         (message (apply #'format format-string args)))
    (format "[%s] [%s] %s" timestamp level-name message)))

(defun agentic-code-log--write (level format-string &rest args)
  "Write a log message at LEVEL with FORMAT-STRING and ARGS.
Only writes if the current log level permits it.
LEVEL should be a numeric value."
  (let* ((current-level-num (agentic-code-log--level-to-number agentic-code-log-level)))
    (cond
     ((< current-level-num 0) nil) ; Logging is OFF
     ((< level current-level-num) nil) ; Message level too low
     (t
      (let* ((message (apply #'agentic-code-log--format-message level format-string args))
             (face (alist-get level agentic-code-log--level-faces)))
        (with-current-buffer (agentic-code-log--get-buffer)
          (goto-char (point-max))
          (let* ((inhibit-read-only t))
            (insert (propertize (concat message "\n") 'face face)))))))))

;;;; Public Functions

(defun agentic-code-log-enabled-p ()
  "Return non-nil if logging is enabled."
  (not (null agentic-code-log-level)))

(defun agentic-code-log-set-level (level)
  "Set the logging LEVEL programmatically.
LEVEL can be nil (OFF) or a symbol (debug, info, warn, error, fatal)."
  (interactive
   (list (let* ((choice (completing-read "Log level: "
                                         '("off" "debug" "info" "warn" "error" "fatal")
                                         nil t)))
           (cond
            ((string= choice "off") nil)
            (t (intern choice))))))
  (let* ((valid-levels '(nil debug info warn error fatal)))
    (cond
     ((memq level valid-levels)
      (setq agentic-code-log-level level)
      (message "Log level set to: %s"
               (cond
                ((null level) "OFF")
                (t (upcase (symbol-name level))))))
     (t
      (error "Invalid log level: %s. Must be nil or one of: debug, info, warn, error, fatal" level)))))

(defun agentic-code-log-clear ()
  "Clear the log buffer."
  (interactive)
  (let* ((buffer (get-buffer agentic-code-log-buffer-name)))
    (cond
     (buffer
      (with-current-buffer buffer
        (let* ((inhibit-read-only t))
          (erase-buffer)))
      (message "Log buffer cleared"))
     (t
      (message "No log buffer exists")))))

;;;; Logging Functions

(defun agentic-code-log-debug (format-string &rest args)
  "Log a debug message with FORMAT-STRING and ARGS."
  (apply #'agentic-code-log--write 0 format-string args))

(defun agentic-code-log-info (format-string &rest args)
  "Log an info message with FORMAT-STRING and ARGS."
  (apply #'agentic-code-log--write 1 format-string args))

(defun agentic-code-log-warn (format-string &rest args)
  "Log a warning message with FORMAT-STRING and ARGS."
  (apply #'agentic-code-log--write 2 format-string args))

(defun agentic-code-log-error (format-string &rest args)
  "Log an error message with FORMAT-STRING and ARGS."
  (apply #'agentic-code-log--write 3 format-string args))

(defun agentic-code-log-fatal (format-string &rest args)
  "Log a fatal message with FORMAT-STRING and ARGS."
  (apply #'agentic-code-log--write 4 format-string args))

(provide 'agentic-code-log)
;;; agentic-code-log.el ends here
