;;; lordar-mode-line.el --- Minimal mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/lordar-mode-line
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0.50"))
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

(defgroup lordar-mode-line nil
  "A minimal mode line configuration."
  :group 'lordar-mode-line)

(defcustom lordar-mode-line-checker-symbols
  '(;; Checker format string will be called with number of errors.
    (:checker-info . "i%s")
    (:checker-issues . "e%s")
    (:checker-good . "-")
    (:checker-checking . "*")
    (:checker-errored . "?")
    (:checker-interrupted . "?")
    ;; VC format string will be called with branch.
    (:vc-added . "a-%s")
    (:vc-needs-merge . "m-%s")
    (:vc-needs-update . "u-%s")
    (:vc-conflict . "c-%s")
    (:vc-others . "?-%s")
    (:vc-good . "@-%s")
    ;; Buffer will be called with no argument.
    (:buffer-modified . "*")
    (:buffer-read-only . "#"))
  "A list of symbols used in the mode line for various status indicators.
Each entry is a cons cell where the key is a keyword describing the status,
and the value is a format string representing the status symbol in the mode
line."
  :group 'lordar-mode-line
  :type '(alist :tag "Character"
                :key-type (symbol :tag "Symbol name")
                :value-type (character :tag "Character to use")))

;;;; Faces

(defgroup lordar-mode-line-faces nil
  "Faces used by lordar-mode-line."
  :group 'lordar-mode-line
  :group 'faces)

(defface lordar-mode-line-active
  '((t (:inherit mode-line-active)))
  "Default face used if the mode-line is active."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-inactive
  '((t (:inherit mode-line-inactive)))
  "Default face used if the mode-line is inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-warning
  '((t (:inherit warning)))
  "Default face used if the mode-line is active."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-error
  '((t (:inherit error)))
  "Default face used if the mode-line is inactive."
  :group 'lordar-mode-line-faces)

;;;; Variables

;;;; Helper Functions

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
            (lordar-mode-line-segments-winum
             (lordar-mode-line-segments-vertical-space))
            (lordar-mode-line-segments-vertical-space)
            (lordar-mode-line-segments-evil-state)
            (lordar-mode-line-segments-vertical-space)
            (lordar-mode-line-segments-buffer-status)
            (lordar-mode-line-segments-buffer-name)
            (lordar-mode-line-segments-vertical-space 1.5)
            (lordar-mode-line-segments-project-root-relative-directory)))
          mode-line-format-right-align
          (:eval
           (concat
            (lordar-mode-line-segments-major-mode)
            (lordar-mode-line-segments-vertical-space)))))
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
