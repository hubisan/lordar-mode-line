;;; lordar-mode-line-segments.el --- Segments for lordar-mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann

;; This file is not part of GNU Emacs

;;; License:

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

;; Segments used by lordar-mode-line.

;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'project)
  (require 'vc)
  (require 'pcase))

(eval-when-compile
  (defvar evil-mode-line-tag)
  (defvar winum-auto-setup-mode-line))

(eval-when-compile
  (declare-function winum-get-number-string "ext:winum"))

;;;;

;;;; Helpers

(defun lordar-mode-line-segments--get-face (face)
  "Return the appropriate face for the symbol FACE.
If the selected window is active, return face with `lordar-mode-line-' as
prefix. If inactive, return the corresponding FACE with an additional
`-inactive' suffix."
  (let* ((face (format "lordar-mode-line-%s" (symbol-name face)))
         (active-face (intern-soft face)))
    (if (mode-line-window-selected-p)
        (or active-face
            (user-error "Face %s doesn't exist" face))
      (let* ((inactive-face-string (format "%s-inactive" face))
             (inactive-face (intern-soft inactive-face-string)))
        (or inactive-face
            (user-error "Face %s doesn't exist" inactive-face-string))))))

(defun lordar-mode-line-segments--get-symbol (key symbols)
  "Return the symbol associated with KEY from SYMBOLS alist.
SYMBOLS is the symbol without the prefix `lordar-mode-line' and without
the `symbols' suffix. So `buffer-status' for instance gets turned into
`lordar-mode-line-buffer-status-symbols'."
  (let* ((symbols-string (format "lordar-mode-line-%s-symbols"
                                 (symbol-name symbols)))
         (symbols-alist (symbol-value (intern-soft symbols-string))))
    (if symbols-alist
        (or (alist-get key symbols-alist)
            (user-error "Symbol %s doesn't exist in %s" key symbols-string))
      (user-error "Symbols alist %s doesn't exist" symbols-string))))

(defun lordar-mode-line-segments-propertize (text face)
  "Propertize TEXT with the FACE.
If the selected window is active, set face with `lordar-mode-line-' as prefix.
If inactive, set the corresponding FACE with an additional `-inactive' suffix."
  (propertize text 'face (lordar-mode-line-segments--get-face face)))

;;;; Adjust Height

(defcustom lordar-mode-line-height-adjust-factor 0.2
  "Factor to adjust the height of the mode line by.
The factor must be a number, which is interpreted as a multiple of the height
of the affected text."
  :group 'lordar-mode-line
  :type 'number)

(defface lordar-mode-line-height-adjust
  '((t (:foreground unspecified :background unspecified)))
  "Face for invisible elements that adjust the mode-line height."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-adjust-height (&optional factor)
  "Adjust the mode-line height by FACTORE using invisible spaces.
If FACTOR is not give use `lordar-mode-line-height-adjust'."
  (let* ((factor (or factor lordar-mode-line-height-adjust-factor))
         (top (propertize " " 'display `((space-width 0.01) (raise ,factor))))
         (bottom (propertize " " 'display
                             `((space-width 0.01) (raise ,(* -1 factor))))))
    (propertize (concat top bottom) 'face 'lordar-mode-line-height-adjust)))

;;;; Vertical Space

(defface lordar-mode-line-vertical-space
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the major mode in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vertical-space-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the major mode in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-vertical-space (&optional width)
  "Vertical space with space-width set to WIDTH.
If WIDTH is nil set it to 1."
  (let* ((width (or width 1.0)))
    (propertize " " 'display `((space-width ,width))
                'face (lordar-mode-line-segments--get-face 'vertical-space))))

;;;; Major Mode

(defface lordar-mode-line-major-mode
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the major mode in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-major-mode-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the major mode in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-major-mode ()
  "Pretty name of current buffer's major mode."
  (lordar-mode-line-segments-propertize mode-name 'major-mode))

;;;; Buffer Name

(defface lordar-mode-line-buffer-name
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the value of `buffer-name'."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-name-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the value of `buffer-name' when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-buffer-name ()
  "Return the name of the current buffer."
  (format-mode-line "%b" (lordar-mode-line-segments--get-face 'buffer-name)))

;;;; Buffer Status

(defcustom lordar-mode-line-buffer-status-symbols
  '((buffer-not-modified . "")
    (buffer-modified . "* ")
    ;; %% is needed to print %.
    (buffer-read-only . "%% "))
  "Symbols for buffer status in the mode line.
Each entry is a cons cell with a keyword and a corresponding format string."
  :group 'lordar-mode-line
  :type '(alist :tag "Character"
                :key-type (symbol :tag "Symbol name")
                :value-type (character :tag "Character to use")))

(defface lordar-mode-line-buffer-status
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying buffer status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying buffer status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-modified
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying modified status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-modified-inactive
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying modified status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-read-only
  '((t (:inherit lordar-mode-line-warning)))
  "Face used for displaying read-only status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-read-only-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying read-only status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-buffer-status ()
  "Return an indicator representing the status of the current buffer.
Uses symbols defined in `lordar-mode-line-buffer-status-symbols'."
  (when (buffer-file-name (buffer-base-buffer))
    (let* ((symbol-and-face
            (cond
             (buffer-read-only '(buffer-read-only buffer-status-read-only))
             ((buffer-modified-p) '(buffer-modified buffer-status-modified))
             (t '(buffer-not-modified buffer-status))))
           (symbol (lordar-mode-line-segments--get-symbol
                    (car symbol-and-face) 'buffer-status))
           (face (lordar-mode-line-segments--get-face (cadr symbol-and-face))))
      (propertize symbol 'face face))))

;;;; Project Directory

(defface lordar-mode-line-project-directory
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying buffer status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-project-directory-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying buffer status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-project-root-basename ()
  "Return the project root basename.
If not in a project the basename of `default-directory' is returned."
  (let* ((root
          (if-let* ((project (project-current)))
              (project-root project)
            default-directory)))
    (lordar-mode-line-segments-propertize
     (file-name-nondirectory (directory-file-name (file-local-name root)))
     'project-directory)))

(defun lordar-mode-line-segments-project-root-relative-directory ()
  "Return the directory path relative to the root of the project.
If not in a project the `default-directory' is returned.
Examples:
- With project at ~/.emacs.test the function returns .emacs.test/modules
  if visiting ~/.emacs.test/modules/lang-elisp.el.
- With no project the function returns ~/projects
  if visiting ~/projects/emacs-never-dies.org"
  (let* (directory)
    (if-let* ((project (project-current))
              (root (project-root project))
              (root-parent (file-name-parent-directory root))
              (relative-dir (file-relative-name default-directory
                                                root-parent)))
        (setq directory relative-dir)
      (setq directory default-directory))
    (lordar-mode-line-segments-propertize
     (directory-file-name (abbreviate-file-name
                           (file-local-name directory)))
     'project-directory)))

;;;; Version Control State

(defvar-local lordar-mode-line-segments--vc-text nil
  "Mode line segment string indicating the current state of `vc-mode'.")

;; (defun lordar-mode-line-segments--vc-get-branch (vc-mode-str backend)
;;   "Return name of current file's branch for BACKEND according to `vc-mode'.
;; VC-MODE-STR is expected to be the value of `vc-mode' in the current buffer.
;; If `vc-display-status' is nil, return the name of BACKEND."
;;   (or (unless vc-display-status
;;         (symbol-name backend))
;;       (pcase backend
;;         ('Git (substring-no-properties vc-mode-str 5))
;;         ('Hg (substring-no-properties vc-mode-str 4)))
;;       (ignore-errors
;;         (substring (vc-working-revision buffer-file-name backend) 0 7))
;;       "???"))

;; TODO
;; (defun lordar-mode-line--vc-update (&rest _args)
;;   "Update `mood-line-segment-vc--text' against the current VCS state."
;;   (setq mood-line-segment-vc--text
;;         (when-let* ((vc-active (and vc-mode buffer-file-name))
;;                     (backend (vc-backend buffer-file-name))
;;                     (state (vc-state buffer-file-name))
;;                     (rev (mood-line-segment-vc--rev vc-mode backend)))
;;           (cond
;;            ((memq state '(edited added))
;;             (format #("%s %s"
;;                       0 2 (face mood-line-status-info))
;;                     (mood-line--get-glyph :vc-added)
;;                     rev))
;;            ((eq state 'needs-merge)
;;             (format #("%s %s"
;;                       0 2 (face mood-line-status-warning))
;;                     (mood-line--get-glyph :vc-needs-merge)
;;                     rev))
;;            ((eq state 'needs-update)
;;             (format #("%s %s"
;;                       0 2 (face mood-line-status-warning))
;;                     (mood-line--get-glyph :vc-needs-update)
;;                     rev))
;;            ((memq state '(removed conflict unregistered))
;;             (format #("%s %s"
;;                       0 2 (face mood-line-status-error))
;;                     (mood-line--get-glyph :vc-conflict)
;;                     rev))
;;            (t
;;             (format #("%s %s"
;;                       0 5 (face mood-line-status-neutral))
;;                     (mood-line--get-glyph :vc-good)
;;                     rev))))))

;;;; Input Method

;;;; Syntax-Checking

;;;; Evil State

(defface lordar-mode-line-evil-state
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying buffer status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-evil-state-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying buffer status in the mode line."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-evil-state ()
  "Return the value of `evil-mode-line-tag`."
  (lordar-mode-line-segments-propertize (eval evil-mode-line-tag) 'evil-state))

;;;; Winum (Window Number)

(defface lordar-mode-line-winum
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying `winum' number in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-winum-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying `winum' number in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-winum (&optional padding)
  "Return the winum number string for the mode line, with optional PADDING.
If PADDING is provided, it will be added before and after the winum number.
This function also ensures `winum-auto-setup-mode-line' is disabled."
  (setq winum-auto-setup-mode-line nil)
  (let* ((nr (winum-get-number-string))
         (text (if padding
                   (format "%s%s%s" padding nr padding)
                 nr)))
    (lordar-mode-line-segments-propertize text 'winum)))

(provide 'lordar-mode-line-segments)

;;; lordar-mode-line-segments.el ends here
