;;; test-lordar-mode-line.el --- Tests  -*- lexical-binding:t -*-

;; Tests to check if the database functions are working.

;;; Requirements

(require 'buttercup)
(require 'ert)

(require 'lordar-mode-line)

(require 'project)
(require 'flymake)

(defvar test-buffer nil
  "Buffer for testing stuff inside.")

;;; Helpers

;;; Test Segments

;;;; Auxiliary Functions

(describe ">>> SEGMENTS AUXILIARY FUNCTIONS\n"

  (describe "- lordar-mode-line--segments-get-face"
    :var ((face 'buffer-name))
    (before-each
      (setq-local lordar-mode-line--segments-face-cache
                  (make-hash-table :test 'equal))
      (spy-on 'mode-line-window-selected-p :and-return-value t)
      (spy-on 'intern-soft :and-call-through))

    (it "returns the active face when face is provided and window is active"
      (expect (lordar-mode-line--segments-get-face face)
              :to-be 'lordar-mode-line-buffer-name))

    (it "returns the inactive face when face is provided and window is inactive"
      (spy-on 'mode-line-window-selected-p :and-return-value nil)
      (expect (lordar-mode-line--segments-get-face face)
              :to-be 'lordar-mode-line-buffer-name-inactive))

    (it "returns the default active face when no face is provided and window is active"
      (expect (lordar-mode-line--segments-get-face)
              :to-be 'lordar-mode-line))

    (it "returns the default inactive face when no face is provided and window is inactive"
      (spy-on 'mode-line-window-selected-p :and-return-value nil)
      (expect (lordar-mode-line--segments-get-face)
              :to-be 'lordar-mode-line-inactive))

    (it "caches the face names to avoid repeated intern-soft calls"
      (lordar-mode-line--segments-get-face face)
      (lordar-mode-line--segments-get-face face)
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

;;;; Segments

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
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-adjust-height)))))

      (it "adjusts the height of the mode-line using a provided factor"
        (let* ((factor 0.3)
               (top (propertize " " 'display
                                `((space-width 0.01) (raise ,factor))))
               (bottom (propertize " " 'display
                                   `((space-width 0.01) (raise ,(* -1 factor)))))
               (expected (propertize (concat top bottom) 'face
                                     'lordar-mode-line-height-adjust)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-adjust-height 0.3)))))))

  (describe "> Vertical Space"
    (describe "- lordar-mode-line-segments-vertical-space"

      (it "creates vertical space with default width when no width is provided"
        (let* ((width 1.0)
               (expected (propertize " " 'display `((space-width ,width))
                                     'face 'lordar-mode-line-vertical-space)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-vertical-space)))))

      (it "creates vertical space with provided width"
        (let* ((width 2.0)
               (expected (propertize " " 'display `((space-width ,width))
                                     'face 'lordar-mode-line-vertical-space)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-vertical-space 2.0)))))))

  (describe "> Major Mode"
    (describe "- lordar-mode-line-segments-major-mode"

      (it "returns the major mode name with the correct face and format"
        (spy-on 'format-mode-line :and-return-value "Emacs Lisp")
        (setq-local lordar-mode-line-segments--major-mode nil)
        (let* ((expected (propertize " Emacs Lisp" 'face
                                     'lordar-mode-line-major-mode)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-major-mode " %s")))))

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
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-buffer-name " %s")))))

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
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-buffer-status " %s")))))

      (it "returns buffer status with correct face and format when read-only"
        (spy-on 'format-mode-line :and-return-value "%%")
        (let* ((expected (propertize "%" 'face
                                     'lordar-mode-line-buffer-status-read-only)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-buffer-status)))))))

  (describe "> Project Root"
    (describe "- lordar-mode-line-segments-project-root-basename"

      (it "returns the project root basename with the correct face and format"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value
                '(vc Git "~/projects/coding/lordar-mode-line/"))
        (let* ((expected (propertize " lordar-mode-line" 'face
                                     'lordar-mode-line-project-root-basename)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-project-root-basename " %s")))))

      (it "returns basename of default directory if no project"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value nil)
        (setq-local lordar-mode-line-segments--project-root-basename nil)
        (let* ((default-directory "~/projects/coding/lordar-mode-line/")
               (expected (propertize "lordar-mode-line" 'face
                                     'lordar-mode-line-project-root-basename)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-project-root-basename)))))

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
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-project-root-relative-directory
                    " %s")))))

      (it "returns default directory if no project"
        (spy-on 'lordar-mode-line-segments--project-root-buffer-valid-p
                :and-return-value t)
        (spy-on 'project-current :and-return-value nil)
        (setq-local lordar-mode-line-segments--project-root-relative-directory nil)
        (let* ((default-directory "~/projects/coding/lordar-mode-line/")
               (expected
                (propertize "~/projects/coding/lordar-mode-line" 'face
                            'lordar-mode-line-project-root-relative-directory)))
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-project-root-relative-directory)))))

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
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-vc-branch " %s")))))

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
          (should (equal-including-properties
                   expected
                   (lordar-mode-line-segments-vc-state " %s")))))

      (it "uses the cached value"
        (spy-on 'lordar-mode-line-segments--vc-branch-and-state-update
                :and-call-through)
        (lordar-mode-line-segments-vc-state)
        (expect 'lordar-mode-line-segments--vc-branch-and-state-update
                :to-have-been-called-times 0))))

  (describe "> Input Method"

    (describe "- lordar-mode-line-segments-input-method"

      (it "returns the input methodh with the correct face and format"

        (let* ((inhibit-message t))
          (set-input-method "german")
          (should (equal-including-properties
                   (propertize " DE@" 'face 'lordar-mode-line-input-method)
                   (lordar-mode-line-segments-input-method " %s")))))))

  ;; This test just didn't work with eask test buttercup in Emacs 29.4.
  ;; It works when launching Emacs 29.4 and testing it inside. The line that
  ;; made the problem was (flymake-mode 1). Mabye the diagnostic backens are not
  ;; available. Disabling it for 29.4, just can't solve this.
  (when (>= emacs-major-version 30)

    (describe "> Syntax Checking"

      (before-all
        (setq warning-minimum-log-level :error)
        (setq test-buffer (generate-new-buffer "test-me.el"))
        (switch-to-buffer test-buffer)
        (emacs-lisp-mode)
        (flymake-mode 1)
        (setq-local elisp-flymake-byte-compile t)
        (insert "sdjdfsj\n(require 'nonexistant\n")
        (flymake-start)
        (sleep-for 1))

      (after-all
        (kill-buffer test-buffer)
        (setq warning-minimum-log-level :warning))

      (describe "- lordar-mode-line-segments-syntax-checking-error-counter"

        (it "returns the error counter with correct format"
          (should (equal-including-properties
                   (propertize "Errors: 1" 'face 'lordar-mode-line-syntax-checking-error)
                   (lordar-mode-line-segments-syntax-checking-error-counter "Errors: %s")))))

      (describe "- lordar-mode-line-segments-syntax-checking-warning-counter"

        (it "returns the warning counter with correct format"
          (should (equal-including-properties
                   (propertize "Warnings: 2" 'face 'lordar-mode-line-syntax-checking-warning)
                   (lordar-mode-line-segments-syntax-checking-warning-counter "Warnings: %s")))))

      (describe "- lordar-mode-line-segments-syntax-checking-note-counter"

        (it "returns nil if an error type has zero counts"
          (expect (lordar-mode-line-segments-syntax-checking-note-counter "Notes: %s") :to-be nil))

        (it "returns zero if variable is set to allow it and use special face for zero"
          (should (equal-including-properties
                   (propertize "Notes: 0" 'face 'lordar-mode-line-syntax-checking-zero-counter)
                   (lordar-mode-line-segments-syntax-checking-note-counter "Notes: %s" t t))))

        (it "returns zero if variable is set to allow it and normal face for zero"
          (should (equal-including-properties
                   (propertize "Notes: 0" 'face 'lordar-mode-line-syntax-checking-note)
                   (lordar-mode-line-segments-syntax-checking-note-counter "Notes: %s" t nil))))

        (it "uses the cached value"
          (spy-on 'lordar-mode-line-segments--syntax-checking-counters-update
                  :and-call-through)
          (lordar-mode-line-segments-syntax-checking-note-counter)
          (expect 'lordar-mode-line-segments--syntax-checking-counters-update
                  :to-have-been-called-times 0)))))

  ;; Not testing evil state as it uses the function directly.

  ;; Same for winum.
  )

;;; Test Main Functions

(describe ">>> MAIN FUNCTIONS\n"

  (describe "- lordar-mode-line-mode"

    :var ((minimal-format
           `("%e" (:eval (lordar-mode-line--construct-string
                          ',lordar-mode-line-minimal-segments)))))

    (it "activates and deactivates correctly"
      (expect lordar-mode-line-mode :to-be nil)
      (lordar-mode-line-mode 1)
      (expect lordar-mode-line-mode :to-be t)
      (lordar-mode-line-mode -1)
      (expect lordar-mode-line-mode :to-be nil))

    (it "sets the mode line correctly"
      (lordar-mode-line-set-mode-line lordar-mode-line-minimal-segments)
      (expect mode-line-format :to-equal minimal-format))

    (it "sets major mode specific segments"
      (lordar-mode-line-mode 1)
      (with-current-buffer (get-buffer-create "*test-lordar-mode-line*")
        (special-mode)
        (expect mode-line-format :to-equal minimal-format)))

    (it "sets up hooks correctly"
      (lordar-mode-line-mode 1)
      (expect (memq 'lordar-mode-line--set-major-mode-specific
                    (default-value 'find-file-hook))
              :to-be-truthy)
      (expect (memq 'lordar-mode-line--set-major-mode-specific
                    (default-value 'after-change-major-mode-hook))
              :to-be-truthy)
      (expect (memq 'lordar-mode-line-segments--vc-branch-and-state-update
                    (default-value 'find-file-hook))
              :to-be-truthy)
      (expect (memq 'lordar-mode-line-segments--vc-branch-and-state-update
                    (default-value 'after-save-hook))
              :to-be-truthy))

    (it "sets up advices correctly"
      (lordar-mode-line-mode 1)
      (expect (advice-member-p 'lordar-mode-line-segments--vc-branch-and-state-update
                               'vc-refresh-state)
              :to-be-truthy)
      (expect (advice-member-p 'lordar-mode-line-segments--syntax-checking-counters-update
                               'flymake--handle-report)
              :to-be-truthy)
      (expect (advice-member-p 'lordar-mode-line-segments--syntax-checking-counters-update
                               'flymake-start)
              :to-be-truthy))))

(provide 'test-lordar-mode-line)

;;; test-lordar-mode-line.el ends here
