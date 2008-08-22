;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; css-mode
;; http://www.garshol.priv.no/download/software/css-mode/css-mode.el
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)
