;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; [2008-03-13]
;; シェルはいろいろあるけど現在はterm-modeを使用。

;; term-mode
;; http://dev.ariel-networks.com/Members/matsuyama/pseudo-screen-on-emacs-with-elscreen-and-term
(require 'term)

;; term-modeで、C-xがC-cに割り当てられるのを防ぐ
(term-set-escape-char ?\C-x)

;; term-modeが、C-z(elscreenのプレフィックスキー)をパクるのを防ぐ
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-map "\C-z"
               (lookup-key (current-global-map) "\C-z"))))

;; shell-mode
;; パスワードを隠す
;; http://www.namazu.org/~tsuchiya/elisp/#shell-mode
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; エスケープシーケンスを正しく処理する
;; http://www.namazu.org/~tsuchiya/elisp/#shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; eshell
;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9
(require 'eshell)

;; C-aをbegnning-of-lineではなく、コマンド文字列の先頭に行くようにする
(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)))

;; .*をzshと同じくようにマッチさせる
;; デフォルトでは、.*で親ディレクトリにマッチする
;; http://d.hatena.ne.jp/Rommy/20070115/1168876829
(setq eshell-glob-include-dot-dot nil)

;; Meadow Memo Wikiより
;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9#content_1_31
(setq eshell-ask-to-save-history (quote always))
(setq eshell-history-size 100000)
