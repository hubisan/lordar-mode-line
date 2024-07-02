; -*- mode: emacs-lisp; lexical-binding: t -*-

(eval-when-compile
  (defvar sentence-end-double-space)
  (defvar checkdoc-verb-check-experimental-flag))

;; Can run Emacs with the packages installed:
;; emacs --maximized --init-directory '/home/hubisan/projects/coding/lordar-mode-line/.eask/30.0.50'

;; Domain Specific Language
;; https://emacs-eask.github.io/DSL/
(package "lordar-mode-line"
         "0.1.0"
         "Minimal mode-line")

(website-url "https://github.com/hubisan/lordar-mode-line")
(keywords "mode-line" "faces")
(license "GPLv3")

(package-file "lordar-mode-line.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

;; https://emacs-eask.github.io/DSL/#-source-alias
(source "gnu")
(source "melpa")

;; https://emacs-eask.github.io/DSL/#-depends-on-package-name-rest-recipe
(depends-on "emacs" "29.4")

;; https://emacs-eask.github.io/DSL/#-development-rest-body
(development
 (depends-on "buttercup")
 (depends-on "elsa"))

;; Tell checkdoc not to demand two spaces after a period.
(setq sentence-end-double-space nil)

;; Dont' check docstring for correct verb use.
(setq checkdoc-verb-check-experimental-flag nil)
