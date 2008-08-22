;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 分割したウィンドウでも折り返し
;; C-x 3した時も折り返されるようになる
(setq truncate-partial-width-windows nil)

;; [2008-03-13]
;; これ使ってないなー。

;; windmove
;; http://hovav.net/elisp/
(require 'windmove)
(setq windmove-wrap-around t)
(global-set-key "\C-\M-h" 'windmove-left)
(global-set-key "\C-\M-j" 'windmove-down)
(global-set-key "\C-\M-k" 'windmove-up)
(global-set-key "\C-\M-l" 'windmove-right)

;; C-x p/C-x oでウィンドウ間を移動する。
;; http://www.fan.gr.jp/~ring/Meadow/meadow.html#previous-window
(define-key ctl-x-map "p"
    #'(lambda (arg) (interactive "p") (other-window (- arg))))

;; 画面を1行ずつスクロールする + 物理行単位での移動
;; http://www.bookshelf.jp/soft/meadow_31.html#SEC420
(load "ce-scroll")
(setq ce-smooth-scroll nil)
(setq scroll-preserve-screen-position nil)
