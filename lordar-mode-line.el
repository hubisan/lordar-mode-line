;;; lordar-mode-line.el --- Minimal mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/lordar-mode-line
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: mode-line faces

;; This file is not part of GNU Emacs

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

;; Lordar-mode-line is a minimal mode-line.
;; It is inspired by mood-line.
;;
;; To activate use (lordar-mode-line-mode).
;;
;; The default mode-line will be activated for all buffers.
;; If you want to use custom segments in a major-mode you can do as follows:

;;; Code:

;;;; Requirements

(require 'lordar-mode-line-segments)

(eval-when-compile
  (declare-function flymake--handle-report "flymake")
  (declare-function flymake-start "flymake"))

;;;; Customization

;; Segment's customization variables are defined before each segment.

(defgroup lordar-mode-line nil
  "A minimal mode line configuration."
  :group 'lordar-mode-line)

(defcustom lordar-mode-line-default-segments
  '(:left
    ((lordar-mode-line-segments-adjust-height)
     (lordar-mode-line-segments-winum " %s ")
     (lordar-mode-line-segments-evil-state " %s ")
     (lordar-mode-line-segments-buffer-status
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-buffer-name "%s")
     (lordar-mode-line-segments-project-root-relative-directory " %s"))
    :right
    ((lordar-mode-line-segments-vc-state
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-vc-branch "%s ")
     (lordar-mode-line-segments-major-mode "%s ")
     (lordar-mode-line-segments-input-method " %s ")))
  "Default segments used for the mode line.
The :left key defines segment functions or strings to be displayed on the left
side of the mode line, while the :right key defines those for the right side.
Some segment functions support additional arguments."
  :group 'lordar-mode-line
  :type '(plist :key-type (choice (const :left) (const :right))
                :value-type (repeat sexp string)))

(defcustom lordar-mode-line-prog-mode-segments
  '(:left
    ((lordar-mode-line-segments-adjust-height)
     (lordar-mode-line-segments-winum " %s ")
     (lordar-mode-line-segments-evil-state " %s ")
     (lordar-mode-line-segments-buffer-status
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-buffer-name "%s")
     (lordar-mode-line-segments-project-root-relative-directory " %s"))
    :right
    ((lordar-mode-line-segments-syntax-checking-error-counter "%s ")
     (lordar-mode-line-segments-syntax-checking-warning-counter "%s ")
     (lordar-mode-line-segments-syntax-checking-note-counter "%s ")
     (lordar-mode-line-segments-vc-state
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-vc-branch "%s ")
     (lordar-mode-line-segments-major-mode "%s ")))
  "Segments used for the mode line in `prog-mode'.
The :left key defines segment functions or strings to be displayed on the left
side of the mode line, while the :right key defines those for the right side.
Some segment functions support additional arguments."
  :group 'lordar-mode-line
  :type '(plist :key-type (choice (const :left) (const :right))
                :value-type (repeat sexp)))

(defcustom lordar-mode-line-minimal-segments
  '(:left
    ((lordar-mode-line-segments-adjust-height)
     (lordar-mode-line-segments-winum " %s ")
     (lordar-mode-line-segments-evil-state " %s ")
     (lordar-mode-line-segments-buffer-status
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-buffer-name "%s"))
    :right
    ((lordar-mode-line-segments-major-mode "%s ")))
  "Minimal segments used for the mode line.
The :left key defines segment functions or strings to be displayed on the left
side of the mode line, while the :right key defines those for the right side.
Some segment functions support additional arguments."
  :group 'lordar-mode-line
  :type '(plist
          :key-type (choice (const :left) (const :right))
          :value-type (repeat (choice (group :tag "Segment Function and Format"
                                             (function :tag "Segment Function")
                                             (string :tag "Format String"))
                                      (string :tag "String Segment")))))

(defcustom lordar-mode-line-major-mode-definitions
  '((prog-mode . lordar-mode-line-prog-mode-segments)
    ((Info-mode ibuffer-mode special-mode) . lordar-mode-line-minimal-segments))
  "Definition of mode line segments to use per major mode and as default.
The major mode specific segments are added using major mode hooks.
Each key can be a single major mode symbol or a list of major mode symbols.
The corresponding value must be a variable containing the segments."
  :group 'lordar-mode-line
  :type '(alist :key-type (choice (symbol :tag "Major Mode")
                                  (repeat :tag "List of Major Modes" symbol))
                :value-type (symbol :tag "Segments Variable")))

;;;; Faces

;; Segment's faces are defined before each segment.

(defgroup lordar-mode-line-faces nil
  "Faces used by lordar-mode-line."
  :group 'lordar-mode-line
  :group 'faces)

(defface lordar-mode-line
  '((t (:inherit mode-line)))
  "Default face used if the mode-line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-inactive
  '((t (:inherit mode-line-inactive)))
  "Default face used if the mode-line is inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-warning
  `((t (:inherit warning :background ,(or (face-background 'lordar-mode-line)
                                          'unspecified))))
  "Default face used for a warning in the mode-line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-error
  `((t (:inherit error :background ,(or (face-background 'lordar-mode-line)
                                        'unspecified))))
  "Default face used for an error in the mode-line."
  :group 'lordar-mode-line-faces)

;;;; Variables

;;;; Auxiliary Functions

;;;; Cache

(defvar-local lordar-mode-line--segments-cache nil
  "Cache for storing mode line segments.")

(defvar lordar-mode-line--segments-cache-specs
  '((lordar-mode-line-segments-buffer-name)
    (lordar-mode-line-segments-major-mode)
    (lordar-mode-line-segments-project-root-basename)
    (lordar-mode-line-segments-project-root-relative-directory)
    (lordar-mode-line-segments-vertical-space)
    ;; (lordar-mode-line-segments-input-method
    ;;  :hooks (input-method-deactivate-hook))
    )
  "Specification of segments to cache and their invalidation hooks.")

(defun lordar-mode-line--segments-cache-add-invalidation-hooks (segment)
  "Add local hooks to invalidate the cache for the given SEGMENT."
  (let ((hooks (plist-get
                (cdr (assq segment lordar-mode-line--segments-cache-specs))
                :hooks)))
    (when hooks
      (dolist (hook hooks)
        (add-hook
         hook
         (lambda () (lordar-mode-line--segments-cache-invalidate segment))
         nil t)))))

(defun lordar-mode-line--segments-cache-remove-hooks ()
  "Remove all local hooks for cache invalidation."
  (dolist (spec lordar-mode-line--segments-cache-specs)
    (let* ((segment (car spec))
           (hooks (plist-get
                   (cdr (assq segment lordar-mode-line--segments-cache-specs))
                   :hooks)))
      (when hooks
        (dolist (hook hooks)
          (remove-hook
           hook
           (lambda () (lordar-mode-line--segments-cache-invalidate segment))
           t))))))

(defun lordar-mode-line--segments-cache-invalidate (segment)
  "Invalidate the cache for the given SEGMENT."
  (setq-local lordar-mode-line--segments-cache
              (assq-delete-all segment lordar-mode-line--segments-cache)))

(defun lordar-mode-line--segments-cache-set (segment value)
  "Set the SEGMENT to VALUE in the buffer-local mode line cache.
Local hooks will be added to invalidate the cache if necessary."
  (let ((key (or (and (listp segment) (car segment)) segment)))
    (when (assoc key lordar-mode-line--segments-cache-specs)
      (setf (alist-get key lordar-mode-line--segments-cache) value)
      (lordar-mode-line--segments-cache-add-invalidation-hooks key))))

(defun lordar-mode-line--segments-cache-get (segment)
  "Get the value associated with SEGMENT from the buffer-local mode line cache."
  (let ((key (or (and (listp segment) (car segment)) segment)))
    (alist-get key lordar-mode-line--segments-cache)))

;;;; Set Modeline

(defun lordar-mode-line-set-mode-line (&optional segments default)
  "Set the mode line, optionally making it the DEFAULT mode line.
SEGMENTS should be a plist where the :left is a list of segments or strings to
be aligned to the left, and :right contains segments or strings to be aligned
to the right. The resulting string will be padded in the center to fit the width
of the window. If SEGMENTS is nil, the default specification
`lordar-mode-line-default-segments' is used."
  (when-let ((segments (or segments lordar-mode-line-default-segments))
             (modeline
              (list "%e"
                    `(:eval (lordar-mode-line--construct-string ',segments)))))
    (if default
        (setq-default mode-line-format modeline)
      (setq-local mode-line-format modeline))))

(defun lordar-mode-line--set-major-mode-specific (&optional set-default)
  "Set the mode line per major mode if a definition exists.
The definitions can be found in `lordar-mode-line-major-mode-definitions'.
When SET-DEFAULT is non-nil, set the default segments locally."
  (unless (minibuffer-window-active-p (selected-window))
    (let ((found nil))  ;; Flag to track if a match was found
      (catch 'done
        (dolist (entry lordar-mode-line-major-mode-definitions)
          (let ((modes (ensure-list (car entry)))
                (segments (symbol-value (cdr-safe entry))))
            (dolist (mode modes)
              (when (derived-mode-p mode)
                (lordar-mode-line-set-mode-line segments)
                (setq found t)  ;; Set the flag
                (throw 'done t))))))  ;; Exit both loops when a match is found
      ;; If no match was found, set the default mode line
      (when (and set-default (not found))
        (lordar-mode-line-set-mode-line)))))

(defun lordar-mode-line--eval-segment (segment)
  "Evaluate the SEGMENT and return a concatenated string.
If it is a string, propertize it with the default face."
  (if (stringp segment)
      (propertize segment 'face (lordar-mode-line-segments--get-face))
    (let ((cached-value (lordar-mode-line--segments-cache-get segment)))
      (if cached-value
          (let ((face (get-text-property 0 'face cached-value)))
            (when face
              (let* ((face-name (symbol-name face))
                     (cleaned-face-name
                      (replace-regexp-in-string
                       "\\`lordar-mode-line-\\|-inactive\\'" "" face-name)))
                (setq face (intern cleaned-face-name))))
            (propertize cached-value 'face
                        (lordar-mode-line-segments--get-face face)))
        (let ((value (eval segment)))
          (lordar-mode-line--segments-cache-set segment value)
          value)))))

(defun lordar-mode-line--construct-string (segments)
  "Construct a mode line with SEGMENTS which contains left and right parts.
The left part is aligned to the left side and the right part to the right."
  (let* ((left (plist-get segments :left))
         (right (plist-get segments :right))
         (left (when left (mapconcat #'lordar-mode-line--eval-segment left)))
         (right (when right (mapconcat #'lordar-mode-line--eval-segment right)))
         (outside fringes-outside-margins)
         (left-margin (if outside 0.0 1.0))
         (right-fringe (if outside -1.0 0.0))
         (right-margin (if outside -1.0 0.0))
         (padding (propertize
                   " " 'display
                   `(space :align-to
                           (- right-margin
                              (,right-fringe . right-fringe)
                              (,right-margin . right-margin)
                              ,(length right)))
                   'face (lordar-mode-line-segments--get-face))))
    (concat left padding right)))

;;;; Minor-mode

;;;###autoload
(define-minor-mode lordar-mode-line-mode
  "Toggle lordar-mode-line."
  :group 'lordar-mode-line
  :global t
  :lighter nil
  (if lordar-mode-line-mode
      (lordar-mode-line--activate)
    (lordar-mode-line--deactivate)))

(defun lordar-mode-line--activate ()
  "Activate the lordar-mode-line."
  (lordar-mode-line-set-mode-line nil t)
  ;; Change mode line in active buffers.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (lordar-mode-line--set-major-mode-specific t)))
  (advice-add 'vc-refresh-state :after
              #'lordar-mode-line-segments--vc-branch-and-state-update)
  (advice-add #'flymake--handle-report :after
              #'lordar-mode-line-segments--syntax-checking-counters-update)
  (advice-add #'flymake-start :after
            #'lordar-mode-line-segments--syntax-checking-counters-update)
  ;; change-major-mode-hook is called too often, using find file hook instead.
  ;; If you change the major mode in a buffer the mode line will not be changed.
  ;; But this happens rarely.
  (add-hook 'find-file-hook
            #'lordar-mode-line--set-major-mode-specific))

(defun lordar-mode-line--deactivate ()
  "Deactivate the lordar-mode-line."
  (advice-remove 'vc-refresh-state
                 #'lordar-mode-line-segments--vc-branch-and-state-update)
  (advice-remove #'flymake--handle-report
                 #'lordar-mode-line-segments--syntax-checking-counters-update)
  (advice-remove #'flymake-start
            #'lordar-mode-line-segments--syntax-checking-counters-update)
  (remove-hook 'find-file-hook
               #'lordar-mode-line--set-major-mode-specific)
  ;; Restore the old mode-line-format.
  (let* ((original-value (eval (car (get 'mode-line-format 'standard-value)))))
    (setq-default mode-line-format original-value)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq mode-line-format original-value)
        (lordar-mode-line--segments-cache-remove-hooks)))))

(provide 'lordar-mode-line)

;;; lordar-mode-line.el ends here
