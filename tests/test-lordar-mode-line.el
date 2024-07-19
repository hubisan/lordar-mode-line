;;; test-lordar-mode-line.el --- Tests  -*- lexical-binding:t -*-

;; Tests to check if the database functions are working.

;;; Requirements

(require 'buttercup)
(require 'ert)

(require 'lordar-mode-line)

(require 'project)

;;; Helpers

;;; Test Segments

;;;; Auxiliary Functions

(describe ">>> SEGMENTS AUXILIARY FUNCTIONS\n"

  (describe "- lordar-mode-line-segments--get-face"
    :var ((face 'buffer-name))
    (before-each
      (setq-local lordar-mode-line-segments--face-cache
                  (make-hash-table :test 'equal))
      (spy-on 'mode-line-window-selected-p :and-return-value t)
      (spy-on 'intern-soft :and-call-through))

    (it "returns the active face when face is provided and window is active"
      (expect (lordar-mode-line-segments--get-face face)
              :to-be 'lordar-mode-line-buffer-name))

    (it "returns the inactive face when face is provided and window is inactive"
      (spy-on 'mode-line-window-selected-p :and-return-value nil)
      (expect (lordar-mode-line-segments--get-face face)
              :to-be 'lordar-mode-line-buffer-name-inactive))

    (it "returns the default active face when no face is provided and window is active"
      (expect (lordar-mode-line-segments--get-face)
              :to-be 'lordar-mode-line))

    (it "returns the default inactive face when no face is provided and window is inactive"
      (spy-on 'mode-line-window-selected-p :and-return-value nil)
      (expect (lordar-mode-line-segments--get-face)
              :to-be 'lordar-mode-line-inactive))

    (it "caches the face names to avoid repeated intern-soft calls"
      (lordar-mode-line-segments--get-face face)
      (lordar-mode-line-segments--get-face face)
      (expect 'intern-soft :to-have-been-called-times 1)))

  (describe "- lordar-mode-line-segments--get-symbol"
    (before-each
      ;; Mock the symbols alist
      (setq lordar-mode-line-buffer-status-symbols
            '((buffer-not-modified . nil)
              (buffer-modified . "*")
              (buffer-read-only . "%%"))))

    (it "returns the symbol associated with the key when it exists"
      (expect (lordar-mode-line-segments--get-symbol 'buffer-modified 'buffer-status)
              :to-equal "*"))

    (it "raises an error when the symbols alist doesn't exist"
      (expect (lordar-mode-line-segments--get-symbol 'buffer-modified 'non-existent-status)
              :to-throw 'user-error))

    (it "raises an error when the key doesn't exist in the symbols alist"
      (expect (lordar-mode-line-segments--get-symbol 'non-existent-key 'buffer-status)
              :to-throw 'user-error))))

(describe ">>> SEGMENTS\n"

  (describe "> Adjust Height"
    (describe "- lordar-mode-line-segments-adjust-height"
      (before-each
        ;; Ensure default custom variables are set
        (setq lordar-mode-line-height-adjust-factor 0.2))

      (it "adjusts the height of the mode-line using the default factor"
        (let* ((factor 0.2)
               (top (propertize " " 'display
                                `((space-width 0.01) (raise ,factor))))
               (bottom (propertize " " 'display
                                   `((space-width 0.01) (raise ,(* -1 factor)))))
               (expected (propertize (concat top bottom) 'face
                                     'lordar-mode-line-height-adjust)))
          (expect (lordar-mode-line-segments-adjust-height)
                  :to-equal expected)))

      (it "adjusts the height of the mode-line using a provided factor"
        (let* ((factor 0.3)
               (top (propertize " " 'display
                                `((space-width 0.01) (raise ,factor))))
               (bottom (propertize " " 'display
                                   `((space-width 0.01) (raise ,(* -1 factor)))))
               (expected (propertize (concat top bottom) 'face
                                     'lordar-mode-line-height-adjust)))
          (expect (lordar-mode-line-segments-adjust-height 0.3)
                  :to-equal expected)))))

  (describe "> Vertical Space"
    (describe "- lordar-mode-line-segments-vertical-space"

      (it "creates vertical space with default width when no width is provided"
        (let* ((width 1.0)
               (expected (propertize " " 'display `((space-width ,width))
                                     'face 'lordar-mode-line-vertical-space)))
          (expect (lordar-mode-line-segments-vertical-space)
                  :to-equal expected)))

      (it "creates vertical space with provided width"
        (let* ((width 2.0)
               (expected (propertize " " 'display `((space-width ,width))
                                     'face 'lordar-mode-line-vertical-space)))
          (expect (lordar-mode-line-segments-vertical-space 2.0)
                  :to-equal expected)))))

  (describe "> Major Mode"
    (describe "- lordar-mode-line-segments-major-mode"

      (it "returns the major mode name with the correct face and format"
        (spy-on 'format-mode-line :and-return-value "Emacs Lisp")
        (setq-local lordar-mode-line-segments--major-mode nil)
        (let* ((expected (propertize " Emacs Lisp" 'face
                                     'lordar-mode-line-major-mode)))
          (expect (lordar-mode-line-segments-major-mode " %s")
                  :to-equal expected)))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--major-mode-update :and-call-through)
        (lordar-mode-line-segments-major-mode)
        (expect 'lordar-mode-line-segments--major-mode-update
                :to-have-been-called-times 0))))

  (describe "> Buffer Name"
    (describe "- lordar-mode-line-segments-buffer-name"

      (it "returns the buffer name with the correct face and format"
        (spy-on 'buffer-name :and-return-value "test-buffer")
        (setq-local lordar-mode-line-segments--buffer-name nil)
        (let* ((expected (propertize " test-buffer" 'face
                                     'lordar-mode-line-buffer-name)))
          (expect (lordar-mode-line-segments-buffer-name " %s")
                  :to-equal expected)))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--buffer-name-update :and-call-through)
        (lordar-mode-line-segments-buffer-name)
        (expect 'lordar-mode-line-segments--buffer-name-update
                :to-have-been-called-times 0))))

  (describe "> Buffer Status"
    (describe "- lordar-mode-line-segments-buffer-status"
      (before-each
        (spy-on 'buffer-file-name :and-return-value t)
        (setq lordar-mode-line-buffer-status-symbols
              '((buffer-not-modified . nil)
                (buffer-modified . "*")
                (buffer-read-only . "%"))))

      (it "returns no buffer status when not modified"
        (spy-on 'format-mode-line :and-return-value "--")
        (expect (lordar-mode-line-segments-buffer-status) :to-equal nil))

      (it "returns buffer status with correct face and format when modified"
        (spy-on 'format-mode-line :and-return-value "**")
        (let* ((expected (propertize " *" 'face
                                     'lordar-mode-line-buffer-status-modified)))
          (expect (lordar-mode-line-segments-buffer-status " %s")
                  :to-equal expected)))

      (it "returns buffer status with correct face and format when read-only"
        (spy-on 'format-mode-line :and-return-value "%%")
        (let* ((expected (propertize "%" 'face
                                     'lordar-mode-line-buffer-status-read-only)))
          (expect (lordar-mode-line-segments-buffer-status)
                  :to-equal expected)))))

  (describe "> Project Root"
    (describe "- lordar-mode-line-segments-project-root-basename"

      (it "returns the project root basename with the correct face and format"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value
                '(vc Git "~/projects/coding/lordar-mode-line/"))
        (let* ((expected (propertize " lordar-mode-line" 'face
                                     'lordar-mode-line-project-root-basename)))
          (expect (lordar-mode-line-segments-project-root-basename " %s")
                  :to-equal expected)))

      (it "returns basename of default directory if no project"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value nil)
        (setq-local lordar-mode-line-segments--project-root-basename nil)
        (let* ((default-directory "~/projects/coding/lordar-mode-line/")
               (expected (propertize "lordar-mode-line" 'face
                                     'lordar-mode-line-project-root-basename)))
          (expect (lordar-mode-line-segments-project-root-basename)
                  :to-equal expected)))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--project-root-basename-update
                :and-call-through)
        (lordar-mode-line-segments-project-root-basename)
        (expect 'lordar-mode-line-segments--project-root-basename-update
                :to-have-been-called-times 0)))

    (describe "- lordar-mode-line-segments-project-root-relative-directory"

      (it "returns the project root relative directory with the correct face and format"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value
                '(vc Git "~/projects/coding/lordar-mode-line/"))
        (let* ((default-directory "~/projects/coding/lordar-mode-line/tests/")
               (expected
                (propertize " lordar-mode-line/tests" 'face
                            'lordar-mode-line-project-root-relative-directory)))
          (expect (lordar-mode-line-segments-project-root-relative-directory
                   " %s")
                  :to-equal expected)))

      (it "returns default directory if no project"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value nil)
        (setq-local lordar-mode-line-segments--project-root-relative-directory nil)
        (let* ((default-directory "~/projects/coding/lordar-mode-line/")
               (expected
                (propertize "~/projects/coding/lordar-mode-line" 'face
                            'lordar-mode-line-project-root-relative-directory)))
          (expect (lordar-mode-line-segments-project-root-relative-directory)
                  :to-equal expected)))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--project-root-relative-directory-update
                :and-call-through)
        (lordar-mode-line-segments-project-root-relative-directory)
        (expect 'lordar-mode-line-segments--project-root-basename-update
                :to-have-been-called-times 0))))

  (describe "> Version Control"

    (describe "- lordar-mode-line-segments-vc-branch"

      (before-each
        (setq vc-mode " Git-develop")
        (setq buffer-file-name
              "/home/test//lordar-mode-line/lordar-mode-line-segments")
        (spy-on 'vc-backend :and-return-value 'Git))

      (it "returns the VC branch with the correct face and format"
        (setq lordar-mode-line-segments--vc-branch-and-state nil)
        (let* ((expected (propertize " develop" 'face 'lordar-mode-line-vc-branch)))
          (expect (lordar-mode-line-segments-vc-branch " %s")
                  :to-equal expected)))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--vc-branch-and-state-update
                :and-call-through)
        (lordar-mode-line-segments-vc-branch)
        (expect 'lordar-mode-line-segments--vc-branch-and-state-update
                :to-have-been-called-times 0)))

    (describe "- lordar-mode-line-segments-vc-state"

      (before-each
        (setq vc-mode " Git-develop")
        (setq buffer-file-name
              "/home/test//lordar-mode-line/lordar-mode-line-segments")
        (spy-on 'vc-backend :and-return-value 'Git)
        (spy-on 'vc-state :and-return-value 'edited))

      (it "returns the VC state with the correct face and format"
        (setq lordar-mode-line-segments--vc-branch-and-state nil)
        (let* ((expected (propertize " *" 'face 'lordar-mode-line-vc-state-dirty)))
          (expect (lordar-mode-line-segments-vc-state " %s")
                  :to-equal expected)))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--vc-branch-and-state-update
                :and-call-through)
        (lordar-mode-line-segments-vc-state)
        (expect 'lordar-mode-line-segments--vc-branch-and-state-update
                :to-have-been-called-times 0))))
  )

(provide 'test-lordar-mode-line)

;;; test-lordar-mode-line.el ends here
