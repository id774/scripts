;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; Mac用設定
;; http://www.pqrs.org/~tekezo/macosx/doc/unix/
(setq mac-command-key-is-meta t)
(setq mac-command-modifier-meta t)
(setq mac-option-modifier 'meta)
(setq grep-find-use-xargs 'bsd)
(setq browse-url-generic-program "open")
(setq initial-frame-alist '((width . 177) (height . 47) (top . 0) (left . 0)))

;; Ctrl/Cmd/Optionがシステムに渡されるのを防ぐ
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)

;; 半透明化パッチ適用
;; http://homepage.mac.com/matsuan_tamachan/emacs/TransparencyPatch.html
(setq default-frame-alist
      (append (list '(alpha . (90 90))) default-frame-alist))
