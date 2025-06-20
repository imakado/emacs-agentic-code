;;; agentic-code-window-layout-config.el --- Window layout configuration using window-layout.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Imakado

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

;; This file provides window layout configuration for Agentic Code using
;; the window-layout.el package.  It creates a 3-column layout for
;; efficient development workflows.
;;
;; Layout structure:
;;   - Left column: Previous buffer (work history)
;;   - Center column: Current buffer (work context)
;;   - Right column: Claude vterm buffers
;;   - Edit buffers: Displayed at bottom when needed
;;
;; Usage:
;;
;; Set up the layout:
;;   (agentic-code-layout-setup-3-columns)
;;
;; Reset to default:
;;   (agentic-code-layout-reset)

;;; Code:

;;;; Dependencies

(require 'cl-lib)
(require 'window-layout)

;;;; External Variable Declarations

(defvar agentic-code--source-vterm-buffer)
(defvar agentic-code-claude-vterm-buffer-name-regexp)
(defvar agentic-code-edit-buffer-name-regexp)

;;;; Common Variables

(defvar agentic-code--window-layout-wm nil
  "Window management object for the Agentic Code layout.")

(defvar agentic-code--window-layout-active nil
  "Whether the Agentic Code window layout is currently active.")


;;;; Window Configuration Variables

(defcustom agentic-code-layout-window-height-ratio 0.3
  "Height ratio for Agentic Code windows.

This determines the proportion of frame height allocated to
Agentic Code buffers in horizontal split configurations.
Valid range is 0.2 to 0.5, with 0.3 being a good default."
  :type '(float :tag "Height ratio" :value 0.3)
  :group 'agentic-code)

(defcustom agentic-code-layout-window-select-on-display t
  "Whether to select Agentic Code windows when displayed.

When non-nil, displaying an Agentic Code buffer will also
select its window."
  :type 'boolean
  :group 'agentic-code)

(defcustom agentic-code-layout-window-dedicated t
  "Whether Agentic Code windows should be dedicated.

When non-nil, windows displaying Agentic Code buffers will be
dedicated and won't be used for displaying other buffers."
  :type 'boolean
  :group 'agentic-code)

;;;; Utility Functions

(cl-defun agentic-code-layout--display-buffer-alist-update (buffer-regexp action)
  "Safely update `display-buffer-alist' with BUFFER-REGEXP and ACTION.

BUFFER-REGEXP is a regular expression matching buffer names.
ACTION is a display action specification for `display-buffer'.

This function ensures that existing entries are properly updated
or new entries are added without breaking the configuration."
  (let* ((other-entries (cl-loop for entry in display-buffer-alist
                                 when (not (equal (car entry) buffer-regexp))
                                 collect entry)))
    ;; Update display-buffer-alist
    (setq display-buffer-alist
          (cons (cons buffer-regexp action) other-entries))))

(cl-defun agentic-code-layout--calculate-window-size (ratio dimension)
  "Calculate window size based on RATIO and DIMENSION.

RATIO is the desired proportion (0.0 to 1.0).
DIMENSION is either \\='width or \\='height.

Returns the pixel size for the window."
  (let* ((frame-size (cond
                      ((eq dimension 'width) (frame-pixel-width))
                      ((eq dimension 'height) (frame-pixel-height))
                      (t (error "Invalid dimension: %s" dimension)))))
    (floor (* frame-size ratio))))

(cl-defun agentic-code-layout--find-reusable-window (buffer-or-name)
  "Find a window already displaying BUFFER-OR-NAME.

Returns the window if found in the current frame, nil otherwise."
  (let* ((buffer (get-buffer buffer-or-name)))
    (when buffer
      (get-buffer-window buffer))))

;;;; 2-Column Layout Configuration

;;;;; 2-Column Layout Variables

(defcustom agentic-code-layout-2col-left-size-ratio 0.5
  "Size ratio for the left column in 2-column layout.

Value should be between 0.0 and 1.0."
  :type 'float
  :group 'agentic-code
  :safe (lambda (val) (and (numberp val) (<= 0.0 val 1.0))))

;;;;; 2-Column Layout Functions

(cl-defun agentic-code-layout-2col--create-layout ()
  "Create a 2-column layout.

Returns a window management object."
  (wlf:layout
   `(| (:left-size-ratio ,agentic-code-layout-2col-left-size-ratio)
       2col-context
       2col-terminal)
   '((:name 2col-context)
     (:name 2col-terminal))))

(cl-defun agentic-code-layout-2col--setup-initial-buffers ()
  "Set up initial buffer placement for 2-column layout."
  (let* ((current-buffer (current-buffer)))
    (cond
     ((wlf:window-displayed-p agentic-code--window-layout-wm '2col-context)
      (wlf:set-buffer agentic-code--window-layout-wm '2col-context current-buffer)))))

(cl-defun agentic-code-layout-2col--display-buffer-in-context (buffer _alist)
  "Display BUFFER in the 2-column context window.

BUFFER is the buffer to display.
_ALIST is ignored.

This function is used as a display-buffer action."
  (cond
   ((and agentic-code--window-layout-wm
         (wlf:wset-live-p agentic-code--window-layout-wm)
         (wlf:window-displayed-p agentic-code--window-layout-wm '2col-context))
    (let* ((window (wlf:get-window agentic-code--window-layout-wm '2col-context)))
      (set-window-buffer window buffer)
      window))
   (t
    nil)))

(cl-defun agentic-code-layout-2col--display-buffer-in-terminal (buffer _alist)
  "Display BUFFER in the 2-column terminal window.

BUFFER is the buffer to display.
_ALIST is ignored.

This function is used as a display-buffer action."
  (setq agentic-code--window-layout-wm
        (agentic-code-layout-2col--create-layout))
  ;; Set up initial buffers
  (agentic-code-layout-2col--setup-initial-buffers)
  ;; Update display-buffer-alist
  (agentic-code-layout--update-display-buffer-alist '2-columns)

  (cond
   ((and agentic-code--window-layout-wm
         (wlf:wset-live-p agentic-code--window-layout-wm)
         (wlf:window-displayed-p agentic-code--window-layout-wm '2col-terminal))
    (let* ((window (wlf:get-window agentic-code--window-layout-wm '2col-terminal)))
      (set-window-buffer window buffer)
      window))
   (t
    nil)))

;;;; 3-Column Layout Configuration

;;;;; 3-Column Layout Variables

(defcustom agentic-code-layout-3col-left-size-ratio 0.33
  "Size ratio for the left column in 3-column layout.

Value should be between 0.0 and 1.0."
  :type 'float
  :group 'agentic-code
  :safe (lambda (val) (and (numberp val) (<= 0.0 val 1.0))))

(defcustom agentic-code-layout-3col-center-size-ratio 0.5
  "Size ratio for the center column relative to remaining space in 3-column layout.

Value should be between 0.0 and 1.0."
  :type 'float
  :group 'agentic-code
  :safe (lambda (val) (and (numberp val) (<= 0.0 val 1.0))))


;;;;; 3-Column Layout Functions

(cl-defun agentic-code-layout-3col--create-layout ()
  "Create a 3-column layout.

Returns a window management object."
  (wlf:layout
   `(| (:left-size-ratio ,agentic-code-layout-3col-left-size-ratio)
       3col-history
       (| (:left-size-ratio ,agentic-code-layout-3col-center-size-ratio)
          3col-context
          3col-terminal))
   '((:name 3col-history)
     (:name 3col-context)
     (:name 3col-terminal))))

(cl-defun agentic-code-layout-3col--get-suitable-buffer-for-history ()
  "Get a suitable buffer for the history window.

Returns the previous buffer, excluding vterm, edit, and special buffers.
Falls back to *scratch* if no suitable buffer is found."
  (let* ((current (current-buffer))
         (buffer-list (buffer-list))
         (suitable-buffers (agentic-code-layout--filter-unsuitable-buffers buffer-list)))
    ;; Find the first suitable buffer that's not the current buffer
    (cl-loop for buffer in suitable-buffers
             unless (eq buffer current)
             return buffer
             finally return (agentic-code-layout--get-fallback-buffer))))

(cl-defun agentic-code-layout-3col--setup-initial-buffers ()
  "Set up initial buffer placement for 3-column layout."
  (let* ((current-buffer (current-buffer))
         (history-buffer (agentic-code-layout-3col--get-suitable-buffer-for-history)))
    ;; Place buffers in their respective windows
    (cond
     ((wlf:window-displayed-p agentic-code--window-layout-wm '3col-history)
      (wlf:set-buffer agentic-code--window-layout-wm '3col-history history-buffer)))
    (cond
     ((wlf:window-displayed-p agentic-code--window-layout-wm '3col-context)
      (wlf:set-buffer agentic-code--window-layout-wm '3col-context current-buffer)))))

(cl-defun agentic-code-layout-3col--display-buffer-in-history (buffer _alist)
  "Display BUFFER in the 3-column history window.

BUFFER is the buffer to display.
_ALIST is ignored.

This function is used as a display-buffer action."
  (cond
   ((and agentic-code--window-layout-wm
         (wlf:wset-live-p agentic-code--window-layout-wm)
         (wlf:window-displayed-p agentic-code--window-layout-wm '3col-history))
    (let* ((window (wlf:get-window agentic-code--window-layout-wm '3col-history)))
      (set-window-buffer window buffer)
      window))
   (t
    nil)))

(cl-defun agentic-code-layout-3col--display-buffer-in-context (buffer _alist)
  "Display BUFFER in the 3-column context window.

BUFFER is the buffer to display.
_ALIST is ignored.

This function is used as a display-buffer action."
  (cond
   ((and agentic-code--window-layout-wm
         (wlf:wset-live-p agentic-code--window-layout-wm)
         (wlf:window-displayed-p agentic-code--window-layout-wm '3col-context))
    (let* ((window (wlf:get-window agentic-code--window-layout-wm '3col-context)))
      (set-window-buffer window buffer)
      window))
   (t
    nil)))

(cl-defun agentic-code-layout-3col--display-buffer-in-terminal (buffer _alist)
  "Display BUFFER in the 3-column terminal window.

BUFFER is the buffer to display.
_ALIST is ignored.

This function is used as a display-buffer action."

  (setq agentic-code--window-layout-wm
        (agentic-code-layout-3col--create-layout))
  ;; Set up initial buffers
  (agentic-code-layout-3col--setup-initial-buffers)
  ;; Update display-buffer-alist
  (agentic-code-layout--update-display-buffer-alist '3-columns)

  (cond
   ((and agentic-code--window-layout-wm
         (wlf:wset-live-p agentic-code--window-layout-wm)
         (wlf:window-displayed-p agentic-code--window-layout-wm '3col-terminal))
    (let* ((window (wlf:get-window agentic-code--window-layout-wm '3col-terminal)))
      (set-window-buffer window buffer)
      window))
   (t
    nil)))

(cl-defun agentic-code-layout--display-buffer-edit-bottom (buffer _alist)
  "Display BUFFER at the bottom of the vterm window if available.

BUFFER is the buffer to display.
ALIST is the action alist passed by `display-buffer'.

This function shows the buffer in a window below the vterm window
from which the edit command was invoked.  If no vterm window is
found, falls back to creating a window at the bottom of the frame."
  (let* ((reuse-window (agentic-code-layout--find-reusable-window buffer)))
    (cond
     ;; Reuse existing window if found
     (reuse-window
      (set-window-buffer reuse-window buffer)
      (when agentic-code-layout-window-select-on-display
        (select-window reuse-window))
      reuse-window)
     ;; Create new window at bottom
     (t
      (let* ((vterm-buffer (with-current-buffer buffer
                             agentic-code--source-vterm-buffer))
             (vterm-window (and vterm-buffer
                                (buffer-live-p vterm-buffer)
                                (get-buffer-window vterm-buffer)))
             (parent-window (or vterm-window (frame-root-window)))
             (height (cond
                      (vterm-window
                       (floor (* (window-pixel-height vterm-window) agentic-code-layout-window-height-ratio)))
                      (t
                       (agentic-code-layout--calculate-window-size agentic-code-layout-window-height-ratio 'height))))
             (window (split-window parent-window (- height) 'below t)))
        (set-window-buffer window buffer)
        (when agentic-code-layout-window-dedicated
          (set-window-dedicated-p window t))
        (when agentic-code-layout-window-select-on-display
          (select-window window))
        window)))))

;;;; Common Functions

(cl-defun agentic-code-layout--filter-unsuitable-buffers (buffer-list)
  "Filter out unsuitable buffers from BUFFER-LIST.

Removes vterm buffers, edit buffers, and special buffers."
  (cl-loop for buffer in buffer-list
           unless (or (string-match-p agentic-code-claude-vterm-buffer-name-regexp
                                      (buffer-name buffer))
                      (string-match-p agentic-code-edit-buffer-name-regexp
                                      (buffer-name buffer))
                      (string-match-p "^[ *]" (buffer-name buffer)))
           collect buffer))

(cl-defun agentic-code-layout--get-fallback-buffer ()
  "Get a suitable fallback buffer.

Returns *scratch* buffer or creates one if needed."
  (or (get-buffer "*scratch*")
      (let* ((scratch (get-buffer-create "*scratch*")))
        (with-current-buffer scratch
          (lisp-interaction-mode))
        scratch)))


(cl-defun agentic-code-layout--update-display-buffer-alist (layout-type)
  "Update display-buffer-alist for Agentic Code buffers.

LAYOUT-TYPE specifies the layout configuration to use."
  (cond
   ((eq layout-type '2-columns)
    ;; Configure Claude vterm buffers to appear in terminal window
    (agentic-code-layout--display-buffer-alist-update
     agentic-code-claude-vterm-buffer-name-regexp
     '((agentic-code-layout-2col--display-buffer-in-terminal)
       (inhibit-same-window . t)))

    ;; Configure edit buffers to appear at bottom
    (agentic-code-layout--display-buffer-alist-update
     agentic-code-edit-buffer-name-regexp
     '((agentic-code-layout--display-buffer-edit-bottom)
       (inhibit-same-window . t))))
   ((eq layout-type '3-columns)
    ;; Configure Claude vterm buffers to appear in terminal window
    (agentic-code-layout--display-buffer-alist-update
     agentic-code-claude-vterm-buffer-name-regexp
     '((agentic-code-layout-3col--display-buffer-in-terminal)
       (inhibit-same-window . t)))

    ;; Configure edit buffers to appear at bottom
    (agentic-code-layout--display-buffer-alist-update
     agentic-code-edit-buffer-name-regexp
     '((agentic-code-layout--display-buffer-edit-bottom)
       (inhibit-same-window . t))))
   ;; Future layout types can be added here
   (t
    (error "Unknown layout type: %s" layout-type))))

;;;; Public API

;;;###autoload
(cl-defun agentic-code-layout-setup-2-columns ()
  "Set up a 2-column layout for Agentic Code."
  (interactive)
  ;; Create the layout
  (save-window-excursion
    (setq agentic-code--window-layout-wm
          (agentic-code-layout-2col--create-layout)))

  ;; Update display-buffer-alist
  (agentic-code-layout--update-display-buffer-alist '2-columns)

  (setq agentic-code--window-layout-active t)
  ;; (message "Agentic Code 2-column layout activated")
  )

;;;###autoload
(cl-defun agentic-code-layout-setup-3-columns ()
  "Set up a 3-column layout for Agentic Code."
  (interactive)
  ;; Create the layout
  (save-window-excursion
    (setq agentic-code--window-layout-wm
          (agentic-code-layout-3col--create-layout)))

  ;; Set up initial buffers
  ;; (agentic-code-layout-3col--setup-initial-buffers)

  ;; Update display-buffer-alist
  (agentic-code-layout--update-display-buffer-alist '3-columns)

  (setq agentic-code--window-layout-active t)
  ;; (message "Agentic Code 3-column layout activated")
  )

;;;###autoload
(cl-defun agentic-code-layout-reset ()
  "Reset the window layout to the original state."
  (interactive)
  (cond
   ((not agentic-code--window-layout-active)
    (message "No Agentic Code layout to reset"))
   (t
    ;; Clear layout
    (setq agentic-code--window-layout-wm nil)
    (setq agentic-code--window-layout-active nil)

    (message "Agentic Code layout reset"))))

;;;; Footer

(provide 'agentic-code-window-layout-config)

;;; agentic-code-window-layout-config.el ends here
