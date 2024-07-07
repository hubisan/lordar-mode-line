; -*- mode: emacs-lisp; lexical-binding: t -*-

(eval-when-compile
  (defvar sentence-end-double-space)
  (defvar checkdoc-verb-check-experimental-flag))

;; Domain Specific Language
;; https://emacs-eask.github.io/DSL/
(package "lordar-mode-line"
         "0.1.0"
         "Minimal mode-line")

(website-url "https://github.com/hubisan/lordar-mode-line")
(keywords "mode-line" "faces")
(license "GPLv3")

(package-file "lordar-mode-line.el")
(files "lordar-mode-line-*.el")

;; https://emacs-eask.github.io/DSL/#-source-alias
(source "gnu")
(source "nongnu")
(source "melpa")

;; https://emacs-eask.github.io/DSL/#-depends-on-package-name-rest-recipe
(depends-on "emacs" "30.0.50")

;; https://emacs-eask.github.io/DSL/#-development-rest-body
(development
 (depends-on "buttercup")
 (depends-on "package-lint")
 (depends-on "relint"))

;; Tell checkdoc not to demand two spaces after a period.
(setq sentence-end-double-space nil)

;; Dont' check docstring for correct verb use.
(setq checkdoc-verb-check-experimental-flag nil)
