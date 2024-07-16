;;; test-lordar-mode-line.el --- Tests  -*- lexical-binding:t -*-

;; Tests to check if the database functions are working.

;;; Requirements

(require 'buttercup)
(require 'ert)

(require 'lordar-mode-line)

;;; Helpers

;;; Test Segments

;;;; Auxiliary Functions

(describe "Segments - Auxiliary Functions\n"

  (describe "lordar-mode-line-segments--get-face\n"
    :var ((face 'buffer-name))
    (before-each
      ;; Setup: Initialize the cache and mock necessary functions
      (setq lordar-mode-line-segments--face-cache (make-hash-table :test 'equal))
      (spy-on 'mode-line-window-selected-p :and-return-value t)
      (spy-on 'intern-soft :and-call-fake (lambda (name) (intern name))))

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
      (expect 'intern-soft :to-have-been-called-times 1))))

(provide 'test-lordar-mode-line)

;;; test-lordar-mode-line.el ends here
