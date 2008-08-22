;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; yaml-mode
;; http://yaml-mode.clouder.jp/
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(setq auto-mode-alist
      (append '(("\\.ya?ml$" . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
