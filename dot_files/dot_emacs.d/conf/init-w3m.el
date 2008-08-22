;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(add-to-load-path "~/.emacs.d/elisp/emacs-w3m/")

;; emacs-w3m
(require 'w3m-load)

;; GNU infoなキーバインドにする。
(setq w3m-key-binding 'info)
