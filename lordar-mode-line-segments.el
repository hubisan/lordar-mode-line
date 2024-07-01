;;; lordar-mode-line-segments.el --- Segments for lordar-mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann

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

;; Segments used by lordar-mode-line.

;;; Code:

;;;; Requirements

;;;; Version Control State

;; Just use `(vc-mode-line-state (vc-state (buffer-file-name)))'
;; and get the symbol from the returned list.
;; `(nth 2 (vc-mode-line-state (vc-state (buffer-file-name))))'

(defvar-local lordar-mode-line-segments--vc-text nil
  "Mode line segment string indicating the current state of `vc-mode'.")

(defun lodar-mode-line-segments--vc-get-branch (vc-mode-str backend)
  "Return name of current file's branch for BACKEND according to `vc-mode'.
VC-MODE-STR is expected to be the value of `vc-mode' in the current buffer.
If `vc-display-status' is nil, return the name of BACKEND."
  (or (unless vc-display-status
        (symbol-name backend))
      (pcase backend
        ('Git (substring-no-properties vc-mode-str 5))
        ('Hg (substring-no-properties vc-mode-str 4)))
      (ignore-errors
        (substring (vc-working-revision buffer-file-name backend) 0 7))
      "???"))

(defun mood-line-segments--vc-update (&rest _args)
  "Update `mood-line-segment-vc--text' against the current VCS state."
  (setq mood-line-segment-vc--text
        (when-let* ((vc-active (and vc-mode buffer-file-name))
                    (backend (vc-backend buffer-file-name))
                    (state (vc-state buffer-file-name))
                    (rev (mood-line-segment-vc--rev vc-mode backend)))
          (cond
           ((memq state '(edited added))
            (format #("%s %s"
                      0 2 (face mood-line-status-info))
                    (mood-line--get-glyph :vc-added)
                    rev))
           ((eq state 'needs-merge)
            (format #("%s %s"
                      0 2 (face mood-line-status-warning))
                    (mood-line--get-glyph :vc-needs-merge)
                    rev))
           ((eq state 'needs-update)
            (format #("%s %s"
                      0 2 (face mood-line-status-warning))
                    (mood-line--get-glyph :vc-needs-update)
                    rev))
           ((memq state '(removed conflict unregistered))
            (format #("%s %s"
                      0 2 (face mood-line-status-error))
                    (mood-line--get-glyph :vc-conflict)
                    rev))
           (t
            (format #("%s %s"
                      0 5 (face mood-line-status-neutral))
                    (mood-line--get-glyph :vc-good)
                    rev))))))

;;;; Project Directory

(defun lordar-mode-line--project-directory (filename)
  "Return the project directory of FILE-PATH.
If no project was found return the `default-directory'."
  (let* ((root-directory
          (if-let* ((project (project-current)))
              (project-root project)
            default-directory))
         (directory (directory-file-name
                     (file-local-name root-directory)))
         (directory-name)

    )))


(provide 'lordar-mode-line-segments)

;;; lordar-mode-line-segments.el ends here
