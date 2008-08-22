;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; シンタックスハイライトを有効にする
(global-font-lock-mode t)

;; 対応する括弧をハイライト表示
(show-paren-mode t)

;; いろんな括弧をハイライト表示
;; http://www.fan.gr.jp/~ring/Meadow/meadow.html#mic-paren.el
(autoload 'paren-activate "mic-paren" nil t)
(setq paren-match-face 'bold paren-sexp-mode t)

;; [2008-03-13]
;; なんか鬱陶しいことが多いのでコメントアウト。

;; hightlight-current-line
;; http://www.emacswiki.org/cgi-bin/wiki/HighlightCurrentLine
;; (require 'highlight-current-line)
;; (highlight-current-line-on t)
;; (set-face-attribute 'highlight-current-line-face nil
;;                     :background "black"
;;                     :underline nil)

;; リージョンをハイライト表示
(setq transient-mark-mode t)

;; 検索でマッチした箇所をハイライト表示
(setq search-highlight t)

;; 対話置換でマッチした箇所をハイライト
(setq query-replace-highlight t)

;; 全角スペース、タブ等の様々な空白文字をハイライト
;; Meadow/memoからもらってきたと思われる。
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "gray"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))))
