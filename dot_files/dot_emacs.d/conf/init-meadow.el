;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; Win32上のMeadowでのエンコーディング設定
;; デフォルトのutf8を上書き
(set-terminal-coding-system 'sjis)
(set-keyboard-coding-system 'sjis)
(set-selection-coding-system 'sjis)
(set-file-name-coding-system 'sjis)

;; カラーテーマ
;; https://gna.org/projects/color-theme
(require 'color-theme)
(color-theme-arjen)

;; 開始時にホームディレクトリに移動
;; というか、なんか設定ありそうだけど。
(cd (expand-file-name "~"))

;; tool-barを表示
(menu-bar-mode t)

;; IME の ON/OFF でカーソルの色を変える
;; (cursor-type が box, bar の場合)
(add-hook 'mw32-ime-on-hook
                    (lambda () (set-cursor-color "brown"))) ; ON
(add-hook 'mw32-ime-off-hook
                    (lambda () (set-cursor-color "black"))) ; OFF

;; フォント設定
;; http://taka.no32.tk/diary/20070119.html#p01
(w32-add-font  "consolas" nil)
(w32-change-font
 "consolas"
 '((spec
    ((:char-spec ascii :height any)
     strict
     (w32-logfont "Consolas" 0 -12 400 0 nil nil nil 0 1 3 0))
    ((:char-spec ascii :height any :weight bold)
     strict
     (w32-logfont "Consolas" 0 -12 700 0 nil nil nil 0 1 3 0))
    ((:char-spec ascii :height any :slant italic)
     strict
     (w32-logfont "Consolas" 0 -12 400 0 t nil nil 0 1 3 0))
    ((:char-spec ascii :height any :weight bold :slant italic)
     strict
     (w32-logfont "Consolas" 0 -12 700 0 t nil nil 0 1 3 0))
    ((:char-spec japanese-jisx0208 :height any)
     strict
     (w32-logfont "Consolas" 0 -12 400 0 nil nil nil 128 1 3 49)
     ((spacing . 1)))
    ((:char-spec japanese-jisx0208 :height any :slant italic)
     strict
     (w32-logfont "Consolas" 0 -12 400 0 t nil nil 128 1 3 49)
     ((spacing . 1)))
    ((:char-spec japanese-jisx0208 :height any :weight bold)
     strict
     (w32-logfont "Consolas" 0 -12 700 0 nil nil nil 128 1 3 49)
     ((spacing . 1)))
    ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
     strict
     (w32-logfont "Consolas" 0 -12 700 0 t nil nil 128 1 3 49)
     ((spacing . 1))))))

(set-face-attribute 'variable-pitch nil :font "consolas")

(setq default-frame-alist
      (append (list '(font . "consolas")) default-frame-alist))
