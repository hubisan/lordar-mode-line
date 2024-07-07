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
  (require 'vc))

(eval-when-compile
  (defvar evil-mode-line-tag)
  (defvar winum-auto-setup-mode-line)
  (defvar winum-mode)
  (defvar evil-mode))

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
        (if (assoc key symbols-alist)
            (alist-get key symbols-alist)
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

(defun lordar-mode-line-segments-major-mode (&optional format-string)
  "Return the pretty name of the current buffer's major mode.
Use FORMAT-STRING to change the output."
  (let* ((format-string (or format-string "%s"))
         (mode-name (format format-string mode-name)))
    (lordar-mode-line-segments-propertize mode-name 'major-mode)))

;;;; Buffer Name

;; The name of the buffer.
;; Example: lordar-mode-line-segments.el

(defface lordar-mode-line-buffer-name
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the buffer name in the mode line when active."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-name-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the buffer name in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-buffer-name (&optional format-string)
  "Return the name of the current buffer.
Use FORMAT-STRING to change the output."
  (let* ((format-string (or format-string "%s"))
         (buffer-name (format format-string (buffer-name))))
    (lordar-mode-line-segments-propertize buffer-name 'buffer-name)))

;;;; Buffer Status

;; The status of the buffer can be:
;; - not modified,
;; - modified, or
;; - read-only.
;; The text returned per status is defined in the alist
;; `lordar-mode-line-buffer-status-symbols'.

(defcustom lordar-mode-line-buffer-status-symbols
  '((buffer-not-modified . nil)
    (buffer-modified . "*")
    ;; %% is needed to print %.
    (buffer-read-only . "%%"))
  "Symbols for buffer status in the mode line.
Each entry is a cons cell with a keyword and a corresponding string.
Valid keywords are:
- `buffer-not-modified`: for buffers that are not modified.
- `buffer-modified`: for buffers that are modified.
- `buffer-read-only`: for buffers that are read-only."
  :group 'lordar-mode-line
  :type '(alist :tag "String"
                :key-type
                (choice (const :tag "Buffer not modified" buffer-not-modified)
                        (const :tag "Buffer modified" buffer-modified)
                        (const :tag "Buffer read-only" buffer-read-only))
                :value-type (string :tag "String to use")))

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
  "Face used for displaying the modified status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-modified-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the modified status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-read-only
  '((t (:inherit lordar-mode-line-warning)))
  "Face used for displaying the read-only status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-read-only-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the read-only status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-buffer-status (&optional format-string)
  "Return an indicator representing the status of the current buffer.
Uses symbols defined in `lordar-mode-line-buffer-status-symbols'.
Use FORMAT-STRING to change the output."
  (when (buffer-file-name (buffer-base-buffer))
    (when-let* ((format-string (or format-string "%s"))
                (symbol-and-face
                 (cond
                  (buffer-read-only '(buffer-read-only buffer-status-read-only))
                  ((buffer-modified-p) '(buffer-modified buffer-status-modified))
                  (t '(buffer-not-modified buffer-status))))
                (symbol (lordar-mode-line-segments--get-symbol
                         (car symbol-and-face) 'buffer-status))
                (symbol (format format-string symbol))
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

(defun lordar-mode-line-segments--project-root-buffer-valid-p  ()
  "Check if the current buffer is a valid project buffer.
A buffer is considered valid if it is associated with a file or if it is in
`dired-mode'."
  (or (buffer-file-name)
      (eq major-mode 'dired-mode)))

(defun lordar-mode-line-segments-project-root-basename (&optional format-string)
  "Return the project root basename.
If not in a project the basename of `default-directory' is returned.
Use FORMAT-STRING to change the output."
  (when (lordar-mode-line-segments--project-root-buffer-valid-p)
    (let* ((format-string (or format-string "%s"))
           (root (if-let* ((project (project-current)))
                     (project-root project)
                   default-directory))
           (basename (file-name-nondirectory
                      (directory-file-name (file-local-name root))))
           (basename (format format-string basename)))
      (lordar-mode-line-segments-propertize basename 'project-directory))))

(defun lordar-mode-line-segments-project-root-relative-directory (&optional format-string)
  "Return the directory path relative to the root of the project.
If not in a project the `default-directory' is returned.
Examples:
- With project at ~/.emacs.test the function returns .emacs.test/modules
  if visiting ~/.emacs.test/modules/lang-elisp.el.
- With no project the function returns ~/projects
  if visiting ~/projects/emacs-never-dies.org
Use FORMAT-STRING to change the output."
  (when (lordar-mode-line-segments--project-root-buffer-valid-p)
    (let* ((format-string (or format-string "%s"))
           (directory (if-let* ((project (project-current))
                                (root (project-root project))
                                (root-parent (file-name-parent-directory root))
                                (relative-dir
                                 (file-relative-name default-directory
                                                     root-parent)))
                          relative-dir
                        default-directory))
           (directory (directory-file-name (abbreviate-file-name
                                            (file-local-name directory))))
           (directory (format format-string directory)))
      (lordar-mode-line-segments-propertize directory 'project-directory))))

;;;; Version Control Branch

(defface lordar-mode-line-vc-branch
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the VC branch name in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-branch-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the VC branch name in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-vc-branch (&optional format-string)
  "Return the VC branch name for the current buffer.
Use FORMAT-STRING to change the output."
  (when (and vc-mode buffer-file-name)
    (when-let* ((format-string (or format-string "%s"))
                (backend (vc-backend buffer-file-name))
                (branch (cond
                         ((equal backend 'Git) (substring-no-properties vc-mode 5))
                         ((equal backend 'Hg) (substring-no-properties vc-mode 4))))
                (branch (format format-string branch)))
      (lordar-mode-line-segments-propertize branch 'vc-branch))))

;;;; Version Control State

(defcustom lordar-mode-line-vc-state-symbols
  '((up-to-date . nil)
    ;; File has been edited.
    (edited . "*")
    ;; More recent version in repo.
    (needs-update . "!u")
    ;; File edited and more recent version in repo.
    (needs-merge . "!m")
    ;; Scheduled to go into the repository on the next commit.
    (added . "*")
    ;; Will be deleted on next commit.
    (removed . "x")
    ;; File contains conflicts as the result of a merge.
    (conflict . "!c")
    ;; File is ignored.
    (ignored . "x")
    ;; Will be used for the other states.
    (default . nil))
  "Symbols for buffer status in the mode line.
Each entry is a cons cell with a keyword and a corresponding string.
Valid keywords are:"
  :group 'lordar-mode-line
  :type '(alist :tag "String"
                :key-type
                (choice (const :tag "Buffer not modified" buffer-not-modified)
                        (const :tag "Buffer modified" buffer-modified)
                        (const :tag "Buffer read-only" buffer-read-only))
                :value-type (string :tag "String to use")))

(defface lordar-mode-line-vc-state
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the VC state in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the VC state in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-dirty
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the VC state in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-dirty-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the VC state in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-error
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying the VC state in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-error-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying the VC state in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-vc-state (&optional format-string)
  "Return an indicator representing the status of the current buffer.
Uses symbols defined in `lordar-mode-line-buffer-status-symbols'.
Use FORMAT-STRING to change the output."
  (when (and vc-mode buffer-file-name)
    (when-let* ((format-string (or format-string "%s"))
                (state (vc-state buffer-file-name)))
      (when-let* ((symbol-and-face
                   (cond
                    ((eq state 'up-to-date) '(up-to-date vc-state))
                    ((eq state 'edited) '(edited vc-state-dirty))
                    ((eq state 'needs-update) '(needs-update vc-state-dirty))
                    ((eq state 'needs-merge) '(needs-merge vc-state-dirty))
                    ((eq state 'added) '(needs-merge vc-state-dirty))
                    ((eq state 'removed) '(needs-merge vc-state))
                    ((eq state 'conflict) '(needs-merge vc-state-error))
                    ((eq state 'ignored) '(needs-merge vc-state))
                    (t '(default vc-state))))
                  (symbol (lordar-mode-line-segments--get-symbol
                           (car symbol-and-face) 'vc-state))
                  (symbol (format format-string symbol))
                  (face (lordar-mode-line-segments--get-face
                         (cadr symbol-and-face))))
        (propertize symbol 'face face)))))

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

(defun lordar-mode-line-segments-evil-state (&optional format-string)
  "Return the value of `evil-mode-line-tag'.
Use FORMAT-STRING to change the output."
  (when evil-mode
    (when-let* ((format-string (or format-string "%s"))
                (evil-tag (eval evil-mode-line-tag))
                (evil-tag (format format-string evil-tag)))
      (lordar-mode-line-segments-propertize evil-tag 'evil-state))))

;;;; Winum (Window Number)

(defface lordar-mode-line-winum
  '((t (:inherit lordar-mode-line-active)))
  "Face used for displaying `winum' number in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-winum-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used for displaying `winum' number in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-winum (&optional format-string)
  "Return the winum number string for the mode line, with optional PADDING.
If PADDING is provided, it will be added before and after the winum number.
This function also ensures `winum-auto-setup-mode-line' is disabled.
Use FORMAT-STRING to change the output."
  (setq winum-auto-setup-mode-line nil)
  (when winum-mode
    (when-let* ((format-string (or format-string "%s"))
                (nr (winum-get-number-string))
                (nr (format format-string nr) ))
      (lordar-mode-line-segments-propertize nr 'winum))))

(provide 'lordar-mode-line-segments)

;;; lordar-mode-line-segments.el ends here
