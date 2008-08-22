;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; html-helper-mode
(require 'html-helper-mode)
(setq auto-mode-alist (append '(("\\.html?$" . html-helper-mode)
                                ("\\.tt$" . html-helper-mode))
                              auto-mode-alist))

(add-hook 'html-helper-mode-hook
          '(lambda()
               (abbrev-mode nil)))

;; 勝手に雛形が作られてウザいのでnilにしておく
(setq html-helper-build-new-buffer nil)
