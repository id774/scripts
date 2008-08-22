;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; C-hをBackSpaceに
(global-set-key "\C-h" 'delete-backward-char)

;; [2008-03-12]
;; C-lはanythingに

;; C-x C-lでrecenter
(global-set-key "\C-x\C-l" 'recenter)

;; C-x C-iでリージョンをインデント
(global-set-key "\C-x\C-i" 'indent-region)

;; C-x ?でヘルプ
(global-set-key "\C-x?" 'help)

;; C-x uで全置引数
(global-set-key "\C-xu" 'universal-argument)

;; C-c C-iでインフォを引く
(global-set-key "\C-c\C-i" 'info-lookup-symbol)

;; [2008-03-13]
;; Teaminal.app上ではダメ。

;; M-< M-> の代わりに C-< C-> を使う
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)

;; Terminal.appでC-/が効かないので、M-zで代用
(when run-linux (global-set-key "\M-z" 'undo))
