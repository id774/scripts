;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(add-to-load-path "~/.emacs.d/elisp/vcs")

;; vc-mode
(require 'vc)

;; Usage of vc-mode
;; http://www.emacswiki.org/cgi-bin/wiki/VersionControl

;; SVN

;; vc-svn
(require 'vc-svn)
(add-to-list 'vc-handled-backends 'SVN)

;; psvn
(require 'psvn)

;; SVK

;; vc-svk
(require 'vc-svk)
(add-to-list 'vc-handled-backends 'SVK)

;; [2008-03-13]
;; なんかpsvn-svkが動かないお。。

;; psvn-svk
;; (require 'psvn-svn)
;; (require 'psvn-svk)

;; Git
(require 'git)
(require 'git-blame)
(require 'vc-git)

(setq git-committer-email "kentarok@gmail.com")
(setq git-committer-name "Kentaro Kuribayashi")
(add-to-list 'vc-handled-backends 'GIT)
