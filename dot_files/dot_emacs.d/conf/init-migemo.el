;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; [2008-03-03]
;; Linux/Macともに、あらかじめmigemoをインストールすること。

(require 'migemo)

(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1000)
(setq migemo-use-frequent-pattern-alist t)

(migemo-init)
