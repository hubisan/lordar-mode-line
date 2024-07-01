;;; test-my-package.el --- Tests  -*- lexical-binding:t -*-

;; Tests to check if the database functions are working.

;;; Requirements

(require 'buttercup)
(require 'ert)

(require 'my-package)

;;; Helpers

;;; Tests

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

(provide 'test-my-package)

;;; test-my-package.el ends here
