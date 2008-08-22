;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; [2008-03-03]
;; anything-dabbrev-expandを使うようにしたのでコメントアウト。

;; 動的略語展開
;;(define-key global-map "\C-u" 'dabbrev-expand)

;; dabbrev時に、大文字・小文字を区別しない。
(setq dabbrev-case-fold-search nil)

;; dabbrev時に、補完候補をハイライトする。
;; (require 'dabbrev-highlight)

;; [2008-03-13]
;; 誤爆が多い。どうするのがいいかなー。

;; dabbrevをいろんな単語にマッチさせる
(setq dabbrev-abbrev-char-regexp "[A-z0-9:-]")

;; ac-mode
;; http://taiyaki.org/elisp/ac-mode/
;; 通常の動的略語展開 + URLやファイル名も補完
(load "ac-mode")
(setq ac-mode-exception '(dired-mode hex-mode))
(add-hook 'find-file-hooks 'ac-mode-without-exception)
(setq ac-mode-goto-end-of-word t)

;; 静的略語変換。デフォルト無効
(define-key global-map "\C-c\C-u" 'expand-abbrev)
(read-abbrev-file (expand-file-name "~/.emacs.d/conf/abbrves.el"))
(setq-default abbrev-mode nil)
(setq save-abbrevs t)
