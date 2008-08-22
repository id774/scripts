;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; php-mode
(load-library "php-mode")
(require 'php-mode)
(add-hook 'php-mode-user-hook
          '(lambda ())
          (setq c-basic-offset 4)
          (setq indent-tabs-mode nil)
          )
