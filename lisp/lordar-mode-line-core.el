;;; lordar-mode-line-core.el --- Core functions and variables -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Daniel Hubmann

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

;; This file implements the core functionality.

;;; Code:

;;;; Requirements

(eval-when-compile
  (declare-function flymake--handle-report "flymake")
  (declare-function flymake-start "flymake"))

;;;; Customization

(defgroup lordar-mode-line nil
  "A minimal mode line configuration."
  :group 'lordar-mode-line)

(defcustom lordar-mode-line-default-segments
  '(:left-important
    ((lordar-mode-line-segments-adjust-height)
     (lordar-mode-line-segments-winum " %s ")
     (lordar-mode-line-segments-evil-state " %s ")
     (lordar-mode-line-segments-buffer-status
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-buffer-name "%s "))
    :left
    ((lordar-mode-line-segments-project-root-relative-directory "%s"))
    :right
    ((lordar-mode-line-segments-vertical-space)
     (lordar-mode-line-segments-git-state
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-git-branch "%s ")
     (lordar-mode-line-segments-major-mode "%s ")
     (lordar-mode-line-segments-input-method " %s ")))
  "Default segments used for the mode line.
The :left-important key defines segments that should remain visible even
if space is tight. The :left key defines standard left segments, and
:right defines those for the right side."
  :group 'lordar-mode-line
  :type '(plist :tag "Mode Line Segments"
                :key-type (choice (const :tag "Left Important Segments" :left-important)
                                  (const :tag "Left Segments" :left)
                                  (const :tag "Right Segments" :right))
                :value-type (repeat :tag "Segment Function or String" sexp)))

(defcustom lordar-mode-line-prog-mode-segments
  '(:left-important
    ((lordar-mode-line-segments-adjust-height)
     (lordar-mode-line-segments-winum " %s ")
     (lordar-mode-line-segments-evil-state " %s ")
     (lordar-mode-line-segments-buffer-status
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-buffer-name "%s "))
    :left
    ((lordar-mode-line-segments-project-root-relative-directory "%s"))
    :right
    ((lordar-mode-line-segments-vertical-space)
     (lordar-mode-line-segments-syntax-checking-error-counter "%s ")
     (lordar-mode-line-segments-syntax-checking-warning-counter "%s ")
     (lordar-mode-line-segments-syntax-checking-note-counter "%s ")
     (lordar-mode-line-segments-git-state
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-git-branch "%s ")
     (lordar-mode-line-segments-major-mode "%s ")))
  "Segments used for the mode line in `prog-mode'.
The :left-important key defines segments that should remain visible even
if space is tight. The :left key defines standard left segments, and
:right defines those for the right side."
  :group 'lordar-mode-line
  :type '(plist :tag "Mode Line Segments"
                :key-type (choice (const :tag "Left Important Segments"
                                         :left-important)
                                  (const :tag "Left Segments" :left)
                                  (const :tag "Right Segments" :right))
                :value-type (repeat :tag "Segment Function or String" sexp)))

(defcustom lordar-mode-line-minimal-segments
  '(:left-important
    ((lordar-mode-line-segments-adjust-height)
     (lordar-mode-line-segments-winum " %s ")
     (lordar-mode-line-segments-evil-state " %s ")
     (lordar-mode-line-segments-buffer-status
      (concat "%s" (lordar-mode-line-segments-vertical-space 0.4)))
     (lordar-mode-line-segments-buffer-name "%s "))
    :left nil
    :right
    ((lordar-mode-line-segments-git-branch "%s ")
     (lordar-mode-line-segments-major-mode "%s ")))
  "Minimal segments used for the mode line.
The :left-important key defines segments that should remain visible even
if space is tight. The :left key defines standard left segments, and
:right defines those for the right side."
  :group 'lordar-mode-line
  :type '(plist :tag "Mode Line Segments"
                :key-type (choice (const :tag "Left Important Segments" :left-important)
                                  (const :tag "Left Segments" :left)
                                  (const :tag "Right Segments" :right))
                :value-type (repeat :tag "Segment Function or String" sexp)))

(defcustom lordar-mode-line-major-mode-definitions
  '((prog-mode . lordar-mode-line-prog-mode-segments)
    ((Info-mode ibuffer-mode special-mode) . lordar-mode-line-minimal-segments))
  "Definition of mode line segments to use per major mode.
Each key can be a single major mode symbol or a list of major mode symbols.
The corresponding value must be a variable containing the segments."
  :group 'lordar-mode-line
  :type '(alist :key-type (choice (symbol :tag "Major Mode")
                                  (repeat :tag "List of Major Modes" symbol))
                :value-type (symbol :tag "Segments Variable")))

;;;; Faces

(defgroup lordar-mode-line-faces nil
  "Faces used by lordar-mode-line."
  :group 'lordar-mode-line
  :group 'faces)

(defface lordar-mode-line
  '((t (:inherit mode-line)))
  "Default face used in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-inactive
  '((t (:inherit mode-line-inactive)))
  "Default face used if the mode line is inactive."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-warning
  '((t (:inherit (warning lordar-mode-line))))
  "Default face used for a warnings in the mode line."
  :group 'lordar-mode-line-faces)

(defface lordar-mode-line-error
  '((t (:inherit (error lordar-mode-line))))
  "Default face used for an errors in the mode line."
  :group 'lordar-mode-line-faces)

;;;; Variables

;;;; Auxiliary Functions

;;;;; Face Cache

(defvar lordar-mode-line--segments-face-cache (make-hash-table :test 'equal)
  "Cache for concatenated face names.
This function gets called frequently and this avoids calling `intern-soft' each
time.")

(defun lordar-mode-line--segments-get-face (&optional face)
  "Return the appropriate face for the symbol FACE.
If the selected window is active, return FACE with lordar-mode-line- as prefix.
If inactive, return the corresponding FACE with an additional -inactive suffix.
If FACE is nil, use the default face."
  (if face
      (let* ((active (mode-line-window-selected-p))
             (cache-key (concat (symbol-name face) (if active "" "-inactive")))
             (cached-face (gethash cache-key
                                   lordar-mode-line--segments-face-cache)))
        (or cached-face
            (let ((new-face (intern-soft (concat "lordar-mode-line-"
                                                 cache-key))))
              (puthash cache-key new-face
                       lordar-mode-line--segments-face-cache))))
    (if (mode-line-window-selected-p)
        'lordar-mode-line
      'lordar-mode-line-inactive)))

;;;; Set Modeline

(defun lordar-mode-line-set-mode-line (&optional segments default)
  "Set the mode line, optionally making it the DEFAULT mode line.
SEGMENTS should be a plist where the :left is a list of segments or strings to
be aligned to the left, and :right contains segments or strings to be aligned
to the right. The resulting string will be padded in the center to fit the width
of the window. If SEGMENTS is nil, the default specification
`lordar-mode-line-default-segments' is used."
  (when-let* ((segments (or segments lordar-mode-line-default-segments))
              (modeline
               (list "%e"
                     `(:eval (lordar-mode-line--construct-string ',segments)))))
    (if default
        (setq-default mode-line-format modeline)
      (setq-local mode-line-format modeline))))

(defun lordar-mode-line--set-major-mode-specific ()
  "Set the mode line per major mode if a definition exists.
The definitions can be found in `lordar-mode-line-major-mode-definitions'.
When no match found the default segments are used."
  (unless (minibufferp)
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
      (unless found
        (lordar-mode-line-set-mode-line)))))

(defun lordar-mode-line--eval-segment (segment)
  "Eval the SEGMENT and concacenate into a string.
If it is a string propertize it with the default face."
  (if (stringp segment)
      (propertize segment 'face (lordar-mode-line--segments-get-face))
    (eval segment)))

(defun lordar-mode-line--construct-string (segments)
  "Construct a mode line string from SEGMENTS with left and right alignment.
SEGMENTS is a plist with keys :left-important, :left and :right. Each side
contains a list of evaluated mode line segments. Left-important is priorised if
the lenght of the desired mode-line text exceeds the available width.

Pixel alignment for the right side is done by calculating the necessary padding
using the following steps:

Start at the right margin:
                      |
Add the right margin width, the right fringe width (only if it lies outside
the margins), and the scroll bar width (if enabled), to reach the visible
window edge:
                      |--margin-->|--fringe-->|--scroll bar-->|
Subtract the pixel width of the right segment (in chars, as a float) to get
the position where the right segment should start:
          |<---------------------- right segment text --------|

See Info node `(elisp)Pixel Specification' for more information about the
`:align-to' display specification.

Whether fringes are outside the margins is determined using `window-fringes',
not the variable `fringes-outside-margins', because the window layout can also
be set directly via `set-window-fringes', bypassing that variable."
  (let* ((left-important-segs (plist-get segments :left-important))
         (left-segs (plist-get segments :left))
         (right-segs (plist-get segments :right))
         (face (lordar-mode-line--segments-get-face))
         ;; eval segments (once)
         (left-important-text
          (if left-important-segs
              (mapconcat #'lordar-mode-line--eval-segment left-important-segs)
            ""))
         (left-text
          (if left-segs
              (mapconcat #'lordar-mode-line--eval-segment left-segs)
            ""))
         (right-text
          (if right-segs
              (mapconcat #'lordar-mode-line--eval-segment right-segs)
            ""))
         ;; Not sure if ths is the correct width, but in testing it works.
         (win-width (window-total-width))
         (left-important-width (string-width left-important-text))
         ;; available for: left-text + right-text
         (avail (max 0 (- win-width left-important-width)))
         ;; allocate space: keep right as much as possible
         (right-desired-width (string-width right-text))
         (right-space (min right-desired-width avail))
         (left-space (max 0 (- avail right-space)))
         ;; truncate left (cut on the right)
         (left-text
          (if (> left-space 0)
              (truncate-string-to-width left-text left-space)
            ""))
         ;; Truncate right (cut on the left -> keep the end)
         (right-text
          (if (> left-space 0)
              right-text
            (if (> right-space 0)
                (substring right-text
                           (min (1- right-desired-width)
                                (max 0 (- right-desired-width right-space))))
              "")))
         ;; Pixel-based align-to padding, computed with FINAL right-text
         (fringes-outside-p (nth 2 (window-fringes)))
         (fringe-adjust (if fringes-outside-p -1.0 0.0))
         (right-width-float (/ (string-pixel-width right-text)
                               (float (frame-char-width))))
         (padding
          (propertize
           " " 'display
           `(space :align-to
                   (- right-margin
                      (,fringe-adjust . right-fringe)
                      (-1.0 . right-margin)
                      (-1.0 . scroll-bar)
                      ,right-width-float))
           'face face)))
    (concat left-important-text left-text padding right-text)))

;;;; Setup

(defvar lordar-mode-line-setup-hooks-alist
  '((find-file-hook . lordar-mode-line--set-major-mode-specific)
    (after-change-major-mode-hook . lordar-mode-line--set-major-mode-specific)
    (after-save-hook . lordar-mode-line-segments--buffer-name-invalidate-cache))
  "Alist of hooks and their corresponding setup functions.")

(defun lordar-mode-line--setup-hooks-alist-add (hook-pairs)
  "Add HOOK-PAIRS to `lordar-mode-line-setup-hooks-alist`.
HOOK-PAIRS can be a single hook pair of the form:
  (HOOK . FUNCTION)
or a list of such pairs."
  (if (listp (car-safe hook-pairs))
      (dolist (hook-pair hook-pairs)
        (add-to-list 'lordar-mode-line-setup-hooks-alist hook-pair))
    (add-to-list 'lordar-mode-line-setup-hooks-alist hook-pairs)))

(defvar lordar-mode-line-setup-advices-alist
  '((flymake--handle-report
     :after lordar-mode-line-segments--syntax-checking-counters-update)
    (flymake-start
     :after lordar-mode-line-segments--syntax-checking-counters-update))
  "Alist of functions and their corresponding advice functions and places.")

(defun lordar-mode-line--setup-advices-alist-add (advice-pairs)
  "Add ADVICE-PAIRS to `lordar-mode-line--setup-advices-alist`.
ADVICE-PAIRS can be a single advice pair of the form:
  (FUNCTION :PLACE ADVICE-FUNCTION)
or a list of such pairs."
  (if (listp (car-safe advice-pairs))
      (dolist (advice-pair advice-pairs)
        (add-to-list 'lordar-mode-line-setup-advices-alist advice-pair))
    (add-to-list 'lordar-mode-line-setup-advices-alist advice-pairs)))

(defun lordar-mode-line--setup-hooks (&optional remove)
  "Setup hooks to update some segments.
When REMOVE is non-nil remove the hooks else add them.
Adds or removes the hooks stored in `lordar-mode-line-setup-hooks-alist'."
  (let* ((hook-fn (if remove #'remove-hook #'add-hook)))
    (dolist (hook lordar-mode-line-setup-hooks-alist)
      (funcall hook-fn (car hook) (cdr hook)))))

(defun lordar-mode-line--setup-advices (&optional remove)
  "Setup advices to update some segments.
When REMOVE is non-nil remove the advices else add the advices."
  (dolist (advice lordar-mode-line-setup-advices-alist)
    (let ((fn-to-advice (nth 0 advice))
          (place (nth 1 advice))
          (fn-to-call (nth 2 advice)))
      (if remove
          (advice-remove fn-to-advice fn-to-call)
        (advice-add fn-to-advice place fn-to-call)))))

(defun lordar-mode-line--setup-activate ()
  "Activate the lordar-mode-line."
  ;; Need to disable this to not make it try to add itself to the mode-line.
  (when (bound-and-true-p winum-auto-setup-mode-line)
    (setq winum-auto-setup-mode-line nil))
  (lordar-mode-line-set-mode-line nil t)
  ;; Change mode line in active buffers.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (lordar-mode-line--set-major-mode-specific)))
  (lordar-mode-line--setup-hooks)
  (lordar-mode-line--setup-advices))

(defun lordar-mode-line--setup-deactivate ()
  "Deactivate the lordar-mode-line."
  (lordar-mode-line--setup-hooks 'remove)
  (lordar-mode-line--setup-advices 'remove)
  ;; Restore the old mode-line-format.
  (let* ((original-value (eval (car (get 'mode-line-format 'standard-value)))))
    (setq-default mode-line-format original-value)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq mode-line-format original-value)))))

;;;; Minor-mode

;;;###autoload
(define-minor-mode lordar-mode-line-mode
  "Toggle lordar-mode-line."
  :group 'lordar-mode-line
  :global t
  :lighter nil
  (if lordar-mode-line-mode
      (lordar-mode-line--setup-activate)
    (lordar-mode-line--setup-deactivate)))

(provide 'lordar-mode-line-core)

;;; lordar-mode-line-core.el ends here
