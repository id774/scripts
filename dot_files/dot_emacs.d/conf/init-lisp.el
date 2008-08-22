;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; [2008-03-12]
;; 新規追加。

(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
             (turn-on-eldoc-mode)))
(add-hook 'lisp-interaction-mode-hook
          '(lambda ()
             (define-key lisp-interaction-mode-map "\C-m" 'newline-and-indent)
             (turn-on-eldoc-mode)))
