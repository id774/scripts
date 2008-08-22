;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 新規ファイル作成時に、テンプレートを挿入する。
;; http://d.hatena.ne.jp/ha-tan/20051102/1130993895
(setq  auto-insert-directory "~/.emacs.d/templates/" )
(load "autoinsert" t)
(setq auto-insert-alist
      (append '(("\\.rb"       . "template.rb")
                ("\\.html?"    . "template.html"))
              auto-insert-alist ))
(add-hook 'find-file-hooks 'auto-insert)
