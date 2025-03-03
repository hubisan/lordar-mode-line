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
  (require 'flymake))

(eval-when-compile
  (defvar evil-mode-line-tag)
  (defvar winum-auto-setup-mode-line)
  (defvar winum-mode)
  (defvar evil-mode)
  (defvar current-input-method)
  (defvar current-input-method-title)
  (defvar default-input-method)
  (defvar input-method-alist)
  (defvar mode-line-modified))

(eval-when-compile
  (declare-function winum-get-number-string "ext:winum")
  (declare-function flymake--severity "ext:flymake"))

(require 'lordar-mode-line-core)

;;;; Segments Auxiliary Functions & Variables

(defun lordar-mode-line-segments--get-symbol (key symbols)
  "Return the symbol associated with KEY from SYMBOLS alist.
SYMBOLS is the symbol without the prefix lordar-mode-line and without
the symbols suffix. For instance, buffer-status gets turned into
`lordar-mode-line-buffer-status-symbols'."
  (let* ((symbols-string (concat "lordar-mode-line-" (symbol-name symbols)
                                 "-symbols"))
         (symbols-alist (symbol-value (intern-soft symbols-string))))
    (unless symbols-alist
      (user-error "Symbols alist %s doesn't exist" symbols-string))
    (if (assoc key symbols-alist)
        (alist-get key symbols-alist)
      (user-error "Symbol %s doesn't exist in %s" key symbols-string))))

(defun lordar-mode-line-segments--propertize (text face)
  "Propertize TEXT with the FACE.
If the selected window is active, set FACE with lordar-mode-line- as prefix.
If inactive, set the corresponding FACE with an additional -inactive suffix."
  (propertize text 'face (lordar-mode-line--segments-get-face face)))

;;;; Segment Adjust Height

(defcustom lordar-mode-line-height-adjust-factor 0.2
  "Default factor to adjust the height of the mode line by.
The factor must be a decimal number, which is interpreted as a multiple of the
height of the affected text. And it is added once at the bottom and once at the
top. Just play around with it until it matches what you like."
  :group 'lordar-mode-line
  :type 'number)

(defface lordar-mode-line-height-adjust
  '((t (:foreground unspecified :background unspecified)))
  "Face for invisible elements that adjust the mode line height."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-adjust-height (&optional factor)
  "Adjust the mode-line height by FACTOR using invisible spaces.
If FACTOR is not given, use `lordar-mode-line-height-adjust-factor'."
  (let* ((factor (or factor lordar-mode-line-height-adjust-factor))
         (top (propertize " " 'display `((space-width 0.01) (raise ,factor))))
         (bottom (propertize " " 'display
                             `((space-width 0.01) (raise ,(* -1 factor))))))
    (propertize (concat top bottom) 'face 'lordar-mode-line-height-adjust)))

;;;; Segment Vertical Space

(defface lordar-mode-line-vertical-space
  '((t (:inherit lordar-mode-line)))
  "Face used to display vertical space in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vertical-space-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display vertical space in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-vertical-space (&optional width)
  "Vertical space with space-width set to WIDTH.
If WIDTH is nil, set it to 1."
  (let* ((width (or width 1.0)))
    (propertize " " 'display `((space-width ,width))
                'face (lordar-mode-line--segments-get-face 'vertical-space))))

;;;; Segment Major Mode

(defface lordar-mode-line-major-mode
  '((t (:inherit lordar-mode-line)))
  "Face used to display the major mode in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-major-mode-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the major mode in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defvar-local lordar-mode-line-segments--major-mode nil
  "Cache for the major mode.")

(defun lordar-mode-line-segments--major-mode-update (&optional format-string)
  "Cache the pretty name of the current buffer's major mode.
Use FORMAT-STRING to change the output format."
  (let* ((mode-name (format-mode-line mode-name))
         (mode-name-formatted (if format-string
                                  (format format-string mode-name)
                                mode-name)))
    (setq-local lordar-mode-line-segments--major-mode mode-name-formatted)))

(defun lordar-mode-line-segments-major-mode (&optional format-string)
  "Return the pretty name of the current buffer's major mode.
Use FORMAT-STRING to change the output format."
  ;; Keeping this as the variable sometimes used the default value instead of
  ;; making a local one. This solved it. Trying again without.
  ;; (unless (or (local-variable-p 'lordar-mode-line-segments--major-mode)
  ;;             lordar-mode-line-segments--major-mode)
  ;;   (lordar-mode-line-segments--major-mode-update format-string))
  (unless lordar-mode-line-segments--major-mode
    (lordar-mode-line-segments--major-mode-update format-string))
  (when lordar-mode-line-segments--major-mode
    (lordar-mode-line-segments--propertize lordar-mode-line-segments--major-mode
                                           'major-mode)))

;;;; Segment Buffer Name

(defface lordar-mode-line-buffer-name
  '((t (:inherit lordar-mode-line)))
  "Face used to display the buffer name in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-name-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the buffer name in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defvar-local lordar-mode-line-segments--buffer-name nil
  "Cache for the buffer name.")

(defun lordar-mode-line-segments--buffer-name-update (&optional format-string)
  "Cache the name of the current buffer.
Use FORMAT-STRING to change the output."
  (let* ((buffer-name (buffer-name))
         (buffer-name-formatted (if format-string
                                    (format format-string buffer-name)
                                  buffer-name)))
    (setq-local lordar-mode-line-segments--buffer-name buffer-name-formatted)))

(defun lordar-mode-line-segments-buffer-name (&optional format-string)
  "Return the name of the current buffer.
Use FORMAT-STRING to change the output."
  (unless lordar-mode-line-segments--buffer-name
    (lordar-mode-line-segments--buffer-name-update format-string))
  (when lordar-mode-line-segments--buffer-name
    (lordar-mode-line-segments--propertize lordar-mode-line-segments--buffer-name
                                           'buffer-name)))

(defun lordar-mode-line-segments--buffer-name-invalidate-cache ()
  "Invalidate the cache of the buffer name."
  (setq lordar-mode-line-segments--buffer-name nil))

;;;; Segment Buffer Status

(defcustom lordar-mode-line-buffer-status-symbols
  '((buffer-not-modified . nil)
    (buffer-modified . "*")
    ;; %% is needed to print %.
    (buffer-read-only . "%"))
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
  '((t (:inherit lordar-mode-line)))
  "Face used to display buffer status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display buffer status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-modified
  '((t (:inherit lordar-mode-line-error)))
  "Face used to display the modified status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-modified-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the modified status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-read-only
  '((t (:inherit lordar-mode-line-warning)))
  "Face used to display the read-only status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-buffer-status-read-only-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the read-only status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-buffer-status (&optional format-string)
  "Return an indicator representing the status of the current buffer.
Uses symbols defined in `lordar-mode-line-buffer-status-symbols'.
Use FORMAT-STRING to change the output format."
  (when (buffer-file-name (buffer-base-buffer))
    (when-let* ((status (format-mode-line mode-line-modified))
                (symbol-and-face
                 (cond
                  ((string= status "--") '(buffer-not-modified buffer-status))
                  ((string= status "**") '(buffer-modified
                                           buffer-status-modified))
                  ;; for others %% and %- > as long as read only I don't care
                  ;; about modified.
                  (t '(buffer-read-only buffer-status-read-only))))
                (symbol (lordar-mode-line-segments--get-symbol
                         (car symbol-and-face) 'buffer-status))
                (symbol-formatted (if format-string
                                      (format format-string symbol)
                                    symbol))
                (face-symbol (cadr symbol-and-face)))
      (lordar-mode-line-segments--propertize symbol-formatted face-symbol))))

;;;; Segment Project Root

(defun lordar-mode-line-segments--project-root-buffer-valid-p  ()
  "Check if the current buffer is a valid project buffer.
A buffer is considered valid if it is associated with a file or if it is in
`dired-mode'."
  (or (buffer-file-name)
      (eq major-mode 'dired-mode)))

;;;;; Segment Project Root Basename

(defface lordar-mode-line-project-root-basename
  '((t (:inherit lordar-mode-line)))
  "Face used to display project root basename in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-project-root-basename-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display project root basename in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defvar-local lordar-mode-line-segments--project-root-basename nil
  "Cache for the project root basename.")

(defun lordar-mode-line-segments--project-root-basename-update (&optional format-string)
  "Update the project root basename.
If not in a project the basename of `default-directory' is returned.
Use FORMAT-STRING to change the output."
  (let* ((root (if-let* ((project (project-current)))
                   (project-root project)
                 default-directory))
         (basename (file-name-nondirectory (directory-file-name root)))
         (basename-formatted (if format-string
                                 (format format-string basename)
                               basename)))
    (setq-local lordar-mode-line-segments--project-root-basename
                basename-formatted)))

(defun lordar-mode-line-segments-project-root-basename (&optional format-string)
  "Return the project root basename.
If not in a project the basename of `default-directory' is returned.
Use FORMAT-STRING to change the output."
  (when (lordar-mode-line-segments--project-root-buffer-valid-p)
    (unless lordar-mode-line-segments--project-root-basename
      (lordar-mode-line-segments--project-root-basename-update
       format-string))
    (when lordar-mode-line-segments--project-root-basename
      (lordar-mode-line-segments--propertize
       lordar-mode-line-segments--project-root-basename
       'project-root-basename))))

;;;;; Segment Project Root Relative Directory

(defface lordar-mode-line-project-root-relative-directory
  '((t (:inherit lordar-mode-line)))
  "Face used to display project root relative directory in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-project-root-relative-directory-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display project root relative dir in mode line when inactive."
  :group 'lordar-mode-line-faces)

(defvar-local lordar-mode-line-segments--project-root-relative-directory nil
  "Cache for the project root relative directory.")

(defun lordar-mode-line-segments--project-root-relative-directory-update (&optional format-string)
  "Cache the directory path relative to the root of the project.
If not in a project, the `default-directory' is returned.
Use FORMAT-STRING to change the output format."
  (let* ((project (project-current))
         (directory
          (if project
              (let* ((root (project-root project))
                     (root-parent (file-name-parent-directory root)))
                (file-relative-name default-directory root-parent))
            default-directory))
         (directory (directory-file-name (abbreviate-file-name directory)))
         (directory-formatted (if format-string
                                  (format format-string directory)
                                directory)))
    (setq lordar-mode-line-segments--project-root-relative-directory
          directory-formatted)))

(defun lordar-mode-line-segments-project-root-relative-directory (&optional format-string)
  "Return the directory path relative to the root of the project.
If not in a project, the `default-directory' is returned.
Examples:
- With project at ~/.emacs.test, this function returns .emacs.test/modules
  if visiting ~/.emacs.test/modules/lang-elisp.el.
- With no project, this function returns ~/projects
  if visiting ~/projects/emacs-never-dies.org.
Use FORMAT-STRING to change the output format."
  (when (lordar-mode-line-segments--project-root-buffer-valid-p)
    (unless lordar-mode-line-segments--project-root-relative-directory
      (lordar-mode-line-segments--project-root-relative-directory-update
       format-string))
    (when lordar-mode-line-segments--project-root-relative-directory
      (lordar-mode-line-segments--propertize
       lordar-mode-line-segments--project-root-relative-directory
       'project-root-relative-directory))))

;;;; Segment Version Control

(defvar-local lordar-mode-line-segments--vc-branch-and-state nil
  "Cache for the vc branch and vc state.")

(defun lordar-mode-line-segments--vc-branch-and-state-update (&rest _args)
  "Update `lordar-mode-line-segments--vc-branch-and-state'.
Set vc branch text as car and vc state symbol as cdr."
  (let* ((vc-branch (lordar-mode-line-segments--vc-branch-get))
         (vc-state (lordar-mode-line-segments--vc-state-get)))
    (setq-local lordar-mode-line-segments--vc-branch-and-state
                (list vc-branch vc-state))))

;; Add the necessary advices and hooks:

(lordar-mode-line--setup-hooks-alist-add
 '((find-file-hook . lordar-mode-line-segments--vc-branch-and-state-update)
   (after-save-hook . lordar-mode-line-segments--vc-branch-and-state-update)))

(lordar-mode-line--setup-advices-alist-add
 '(vc-refresh-state
   :after lordar-mode-line-segments--vc-branch-and-state-update))

;;;;; Version Control Branch

(defface lordar-mode-line-vc-branch
  '((t (:inherit lordar-mode-line)))
  "Face used to display the vc branch name in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-branch-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the vc branch name in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments--vc-branch-get ()
  "Return the vc branch name for the current buffer."
  (when (and vc-mode buffer-file-name)
    (when-let* ((backend (vc-backend buffer-file-name)))
      (cond
       ((equal backend 'Git) (substring-no-properties vc-mode 5))
       ((equal backend 'Hg) (substring-no-properties vc-mode 4))))))

(defun lordar-mode-line-segments-vc-branch (&optional format-string)
  "Return the vc branch formatted to display in the mode line.
Use FORMAT-STRING to change the output."
  (unless lordar-mode-line-segments--vc-branch-and-state
    (lordar-mode-line-segments--vc-branch-and-state-update))
  (when-let* ((branch (car-safe lordar-mode-line-segments--vc-branch-and-state))
              (branch-formatted (if format-string
                                    (format format-string branch)
                                  branch)))
    (lordar-mode-line-segments--propertize branch-formatted 'vc-branch)))

;;;;; Version Control State

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
Each entry is a cons cell with a keyword and a corresponding string."
  :group 'lordar-mode-line
  :type '(alist :tag "String"
                :key-type
                (choice (const :tag "Up-to-date" up-to-date)
                        (const :tag "Edited" edited)
                        (const :tag "Needs update" needs-update)
                        (const :tag "Needs merge" needs-merge)
                        (const :tag "Added" added)
                        (const :tag "Removed" removed)
                        (const :tag "Conflict" conflict)
                        (const :tag "Ignored" ignored)
                        (const :tag "Default" default))
                :value-type (string :tag "String to use")))

(defface lordar-mode-line-vc-state
  '((t (:inherit lordar-mode-line)))
  "Face used to display the vc state in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the vc state in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-dirty
  '((t (:inherit lordar-mode-line-warning)))
  "Face used to display a dirty vc state in the mode line.
This is used for edited, needs-update, needs-merge and added."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-dirty-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display dirty vc state in the mode line when inactive.
This is used for edited, needs-update, needs-merge and added."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-error
  '((t (:inherit lordar-mode-line-error)))
  "Face used to display error vc state in the mode line.
This is used for conflicts."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-vc-state-error-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display error vc state in the mode line when inactive.
This is used for conflicts."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments--vc-state-get-symbol (&optional state)
  "Return the symbo for the vc STATE."
  (when (and vc-mode buffer-file-name)
    (when-let* ((state (or state (vc-state buffer-file-name)))
                (symbol (cond ((eq state 'up-to-date) 'up-to-date)
                              ((eq state 'edited) 'edited)
                              ((eq state 'needs-update) 'needs-update)
                              ((eq state 'needs-merge) 'needs-merge)
                              ((eq state 'added) 'added)
                              ((eq state 'removed) 'removed)
                              ((eq state 'conflict) 'conflict)
                              ((eq state 'ignored) 'ignored)
                              (t 'default))))
      (lordar-mode-line-segments--get-symbol symbol 'vc-state))))

(defun lordar-mode-line-segments--vc-state-get ()
  "Return an indicator representing the status of the current buffer.
Uses symbols defined in `lordar-mode-line-buffer-status-symbols'."
  (when (and vc-mode buffer-file-name)
    (when-let* ((state (vc-state buffer-file-name))
                (symbol (lordar-mode-line-segments--vc-state-get-symbol state)))
      (setq-local lordar-mode-line-segments--vc-state-text symbol))))

(defun lordar-mode-line-segments--vc-state-get-face (&optional state)
  "Return the face symbol for the vc STATE."
  (when (and vc-mode buffer-file-name)
    (when-let* ((state (or state (vc-state buffer-file-name))))
      (cond ((memq state '(up-to-date removed ignore)) 'vc-state)
            ((memq state '(edited needs-update needs-merge added))
             'vc-state-dirty)
            ((memq state '(conflict)) 'vc-state-error)
            (t 'vc-state)))))

(defun lordar-mode-line-segments-vc-state (&optional format-string)
  "Return an indicator representing the vc status of the current buffer.
Use FORMAT-STRING to change the output."
  (unless lordar-mode-line-segments--vc-branch-and-state
    (lordar-mode-line-segments--vc-branch-and-state-update))
  (when-let* ((state (car-safe
                      (cdr-safe lordar-mode-line-segments--vc-branch-and-state)))
              (state-formatted (if format-string
                                   (format format-string state)
                                 state))
              (face-symbol (lordar-mode-line-segments--vc-state-get-face)))
    (lordar-mode-line-segments--propertize state-formatted face-symbol)))

;;;; Segment Input Method

(defface lordar-mode-line-input-method
  '((t (:inherit lordar-mode-line)))
  "Face used to display the input method in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-input-method-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display the input method in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-input-method (&optional format-string)
  "Return the current input method.
Use FORMAT-STRING to change the output."
  (when-let* ((input-method (cond
                             (current-input-method current-input-method-title)
                             ((and (bound-and-true-p evil-local-mode)
                                   (bound-and-true-p evil-input-method))
                              (nth 3 (assoc default-input-method
                                            input-method-alist)))))
              (input-method-formatted (if format-string
                                          (format format-string input-method)
                                        input-method)))
    (lordar-mode-line-segments--propertize input-method-formatted
                                           'input-method)))

;;;; Segment Syntax-Checking

(defface lordar-mode-line-syntax-checking-error
  '((t (:inherit lordar-mode-line-error)))
  "Face used to display syntax-checking errors in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-error-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display syntax-checking errors in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-warning
  '((t (:inherit lordar-mode-line-warning)))
  "Face used to display syntax-checking warnings in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-warning-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display syntax-checking warnings in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-note
  '((t (:inherit lordar-mode-line)))
  "Face used to display syntax-checking notes in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-note-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display syntax-checking notes in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-zero-counter
  '((t (:inherit lordar-mode-line)))
  "Face used to display zero counters in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-syntax-checking-zero-counter-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display zero counters in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defvar-local lordar-mode-line-segments--syntax-checking-counters nil
  "Store error, warning and note counter.")

(defun lordar-mode-line-segments--syntax-checking-counters-update (&rest _args)
  "Update `lordar-mode-line-segments--syntax-checking-counters'."
  (let* ((errors (lordar-mode-line-segments--syntax-checking-counter
                  :error))
         (warnings (lordar-mode-line-segments--syntax-checking-counter
                    :warning))
         (notes (lordar-mode-line-segments--syntax-checking-counter
                 :note)))
    (setq-local lordar-mode-line-segments--syntax-checking-counters
                (list errors warnings notes))))

(defun lordar-mode-line-segments--syntax-checking-counter (type)
  "Return counter for TYPE :error, :warning or :note."
  (cond
   ((bound-and-true-p flymake-mode)
    (lordar-mode-line-segments--syntax-checking-flymake-counter type)
    ;; Not using the following as for some reason the cache is not immediately
    ;; udpated. Using a function instead now.
    ;; (cadadr (flymake--mode-line-counter type))
    )
   ((bound-and-true-p flycheck-mode)
    ;; Not implemented as not using anymore.
    (ignore))))

(defun lordar-mode-line-segments--syntax-checking-flymake-counter (type)
  "Return counter for TYPE :error, :warning or :note for flymake.."
  (let* ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (number-to-string count)))

(defun lordar-mode-line-segments--syntax-checking (type &optional format-string
                                                        show-zero use-zero-faces)
  "Returns the number or errors for TYPE formatted for mode line.
TYPE is :error, :warning or :note. Use FORMAT-STRING to change the
output. If SHOW-ZERO is non-nil then also show the counter if it is
zero. If USE-ZERO-FACES is non-nil then use special faces for zero count."
  (unless lordar-mode-line-segments--syntax-checking-counters
    (lordar-mode-line-segments--syntax-checking-counters-update))
  (when-let* ((counters lordar-mode-line-segments--syntax-checking-counters)
              (counter (cond
                        ((eq type :error) (nth 0 counters))
                        ((eq type :warning) (nth 1 counters))
                        ((eq type :note) (nth 2 counters)))))
    (let* ((is-not-0 (> (string-to-number counter) 0)))
      (when-let* ((counter-formatted
                   (when (or is-not-0 show-zero)
                     (if format-string
                         (format format-string counter)
                       counter)))
                  (face (if (and (not is-not-0) use-zero-faces)
                            'syntax-checking-zero-counter
                          (cond
                           ((eq type :error) 'syntax-checking-error)
                           ((eq type :warning) 'syntax-checking-warning)
                           ((eq type :note) 'syntax-checking-note)))))
        (lordar-mode-line-segments--propertize counter-formatted face)))))

(defun lordar-mode-line-segments-syntax-checking-error-counter (&optional
                                                                format-string
                                                                show-zero
                                                                use-zero-faces)
  "Return the errors counter report by enabled syntax checker.
For FORMAT-STRING, SHOW-ZERO and USE-ZERO-FACES see
`lordar-mode-line-segments--syntax-checking'."
  (lordar-mode-line-segments--syntax-checking
   :error format-string show-zero use-zero-faces))

(defun lordar-mode-line-segments-syntax-checking-warning-counter (&optional
                                                                  format-string
                                                                  show-zero
                                                                  use-zero-faces)
  "Return the warnings counter report by enabled syntax checker.
For FORMAT-STRING, SHOW-ZERO and USE-ZERO-FACES see
`lordar-mode-line-segments--syntax-checking'."
  (lordar-mode-line-segments--syntax-checking
   :warning format-string show-zero use-zero-faces))

(defun lordar-mode-line-segments-syntax-checking-note-counter (&optional
                                                               format-string
                                                               show-zero
                                                               use-zero-faces)
  "Return the notes counter report by enabled syntax checker.
For FORMAT-STRING, SHOW-ZERO and USE-ZERO-FACES see
`lordar-mode-line-segments--syntax-checking'."
  (lordar-mode-line-segments--syntax-checking
   :note format-string show-zero use-zero-faces))

;; Add the necessary advices and hooks:

(lordar-mode-line--setup-advices-alist-add
 '((flymake--handle-report
    :after lordar-mode-line-segments--syntax-checking-counters-update)
   (flymake-start
    :after lordar-mode-line-segments--syntax-checking-counters-update)))

;;;; Segment Evil State

(defface lordar-mode-line-evil-state
  '((t (:inherit lordar-mode-line)))
  "Face used to display buffer status in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-evil-state-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display buffer status in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-evil-state (&optional format-string)
  "Return the value of `evil-mode-line-tag'.
Use FORMAT-STRING to change the output."
  (when (bound-and-true-p evil-mode)
    (when-let* ((evil-tag (eval evil-mode-line-tag))
                (evil-tag-formatted (if format-string
                                        (format format-string evil-tag)
                                      evil-tag)))
      (lordar-mode-line-segments--propertize evil-tag-formatted 'evil-state))))

;;;; Segment Winum (Window Number)

(defface lordar-mode-line-winum
  '((t (:inherit lordar-mode-line)))
  "Face used to display `winum' number in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-winum-inactive
  '((t (:inherit lordar-mode-line-inactive)))
  "Face used to display `winum' number in the mode line when inactive."
  :group 'lordar-mode-line-faces)

(defun lordar-mode-line-segments-winum (&optional format-string)
  "Return the winum number string for the mode line.
Use FORMAT-STRING to change the output."
  (setq winum-auto-setup-mode-line nil)
  (when (and (featurep 'winum)
             (bound-and-true-p winum-mode))
    (when-let* ((nr (winum-get-number-string))
                (nr-formatted (if format-string
                                  (format format-string nr)
                                nr) ))
      (lordar-mode-line-segments--propertize nr-formatted 'winum))))

(provide 'lordar-mode-line-segments)

;;; lordar-mode-line-segments.el ends here
