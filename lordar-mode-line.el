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

;;;; Customization

;; Segment's customization variables are defined before each segment.

(defgroup lordar-mode-line nil
  "A minimal mode line configuration."
  :group 'lordar-mode-line)

;;;; Faces

;; Segment's faces are defined before each segment.

(defgroup lordar-mode-line-faces nil
  "Faces used by lordar-mode-line."
  :group 'lordar-mode-line
  :group 'faces)

(defface lordar-mode-line
  '((t (:inherit mode-line-active :background )))
  "Default face used if the mode-line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-inactive
  '((t (:inherit mode-line-inactive)))
  "Default face used if the mode-line is inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-warning
  `((t (:inherit warning :backround ,(face-background 'lordar-mode-line))))
  "Default face used for a warning in the mode-line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-error
  `((t (:inherit error :background ,(face-background 'lordar-mode-line))))
  "Default face used for an error in the mode-line."
  :group 'lordar-mode-line-faces)

;;;; Variables

;;;; Helper Functions

;;;; Make Construct

(defvar lordar-mode-line-left-fringe-width 0
  "Variable to adjust the left side of the mode line if not aligned properly.")

(defvar lordar-mode-line-right-fringe-width 0
  "Variable to adjust the left side of the mode line if not aligned properly.")

(defun lordar-mode-line-set-mode-line (&optional segments default)
  "Set the mode line, optionally making it the DEFAULT mode line.

SEGMENTS should be a list where the `car' is a list of segments or strings to be
aligned to the left, and the optional `cdr'contains segments or strings to be
aligned to the right. The resulting string will be padded in the center to fit
the width of the window. If SEGMENTS is nil, the default specification
`lordar-mode-line-default-segments' is used. "
  (when-let ((modeline
              (list "%e"
                    `(:eval (lordar-mode-line--construct-string ',segments)))))
    (if default
        (setq-default mode-line-format modeline)
      (setq-local mode-line-format modeline))))

(defun lordar-mode-line--eval-segments (segments)
  "Eval the SEGMENTS and concacenate into a string."
  )

(defun lordar-mode-line--construct-string (left &optional right default)
  "Construct a mode line format made of LEFT and RIGHT parts.
Line can be made DEFAULT. Returned string is padded in the center to fit
the width of the window."
  (let* ((format
          `(:eval
            (let* ((left (mapconcat
                          (lambda (element)
                            (if (stringp element)
                                (propertize
                                 element 'face
                                 (lordar-mode-line-segments--get-face))
                              (eval element)))
                          ',left))
                   (right (mapconcat
                           (lambda (element)
                             (if (stringp element)
                                 (propertize
                                  element 'face
                                  (lordar-mode-line-segments--get-face))
                               (eval element)))
                           ',right))
                   (width (window-width))
                   (outside fringes-outside-margins)
                   (left-fringe (if outside -1.0 0.0))
                   (left-margin (if outside 0.0 1.0))
                   (right-fringe (if outside -1.0 0.0))
                   (right-margin (if outside -1.0 0.0))
                   (left-max-size (- width (length right) 2))
                   (left (if (> (length left) left-max-size)
                             (concat
                              (truncate-string-to-width left left-max-size)
                              (propertize
                               "…" 'face
                               (lordar-mode-line-segments--get-face)))
                           left)))
              (concat (propertize
                       " " 'display
                       `(space :align-to
                               (+ left-margin
                                  (,left-fringe . left-fringe)
                                  (,left-margin . left-margin))))
                      (propertize
                       " " 'face 'fringe 'display
                       '(space :width
                               (lordar-mode-line-left-fringe-width)))
                      left
                      (propertize
                       " " 'display
                       `(space :align-to
                               (- right-margin
                                  (,right-fringe . right-fringe)
                                  (,right-margin . right-margin)
                                  (lordar-mode-line-right-fringe-width)
                                  ,(length right))))
                      right
                      (propertize
                       " " 'face 'fringe 'display
                       '(space :width
                               (lordar-mode-line-right-fringe-width))))))))
    (if default
        (setq-default mode-line-format format)
      (setq-local mode-line-format format))))

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
  (setq mode-line-format
        '((:eval
           (concat
            (lordar-mode-line-segments-adjust-height)
            (lordar-mode-line-segments-winum " %s ")
            (lordar-mode-line-segments-evil-state " %s ")
            (lordar-mode-line-segments-buffer-status
             (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
            (lordar-mode-line-segments-buffer-name "%s")
            (lordar-mode-line-segments-project-root-relative-directory " %s")))
          lordar-mode-line-segments-right-align
          (:eval
           (concat
            (lordar-mode-line-segments-syntax-checking-error-counter "%s ")
            (lordar-mode-line-segments-syntax-checking-warning-counter "%s ")
            (lordar-mode-line-segments-syntax-checking-note-counter "%s ")
            (lordar-mode-line-segments-vc-state
             (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
            (lordar-mode-line-segments-vc-branch "%s ")
            (lordar-mode-line-segments-major-mode)
            (lordar-mode-line-segments-vertical-space)
            (lordar-mode-line-segments-input-method " %s ")))))

  ;; VC needs at least an advice, probably no hooks:
  ;; Advice vc-refresh-state
  ;; Find-file hook calls vc-refresh-state, so not needed.
  ;; And on save is probably also not needed. Rather add a hook to call
  ;; vc-refresh-state.

  ;; Do list the buffers
  ;; Apply the mode line depending on the major mode
  ;; Need a list for this and the function
  ;; apply-mode-line-based-on-major-mode
  )

;; I need a list of modes to apply specific mode-lines
;; (defun apply-mode-line-based-on-major-mode ()
;;   "Apply custom mode line based on the current major mode."
;;   (cond
;;    ((derived-mode-p 'prog-mode) (my-mode-line-setup))
;;    ((derived-mode-p 'text-mode) (my-mode-line-setup))
;;    ((derived-mode-p 'text-mode) (my-mode-line-setup))
;;    ;; Add other major modes and their corresponding mode line setups here
;;    (t (my-mode-line-setup))))

(defun lordar-mode-line--deactivate ()
  "Deactivate the lordar-mode-line."
  ;; Restore the old mode-line-format.
  (let* ((original-value (eval (car (get 'mode-line-format 'standard-value)))))
    (setq-default mode-line-format original-value)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq mode-line-format original-value)))))

(provide 'lordar-mode-line)

;;; lordar-mode-line.el ends here
