;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; find-file時にハイライトする
;; http://www.bookshelf.jp/soft/meadow_23.html#SEC217
(setq hc-ctrl-x-c-is-completion t)
(require 'highlight-completion)
(highlight-completion-mode 1)

;; find-file時に、大文字・小文字を区別しない
;; http://d.hatena.ne.jp/khiker/20061220/1166643421
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; pointがある箇所のファイルを開く
(ffap-bindings)

;; M-x時にヒストリを利用
(require 'mcomplete)
(load "mcomplete-history")
(turn-on-mcomplete-mode)

;; [2008-03-03]
;; これ、M-!でできるよ？shell-commandをrequireしてるから？

;; ミニバッファ内でシェルコマンド実行
(require 'shell-command)
(shell-command-completion-mode)

;; ミニバッファのセッションを保存
;; http://d.hatena.ne.jp/higepon/20061230/1167447339
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (setq session-globals-max-string 100000000)
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize))

;; 同名ファイルが複数ある時に、わかりやすくする
;; http://clouder.jp/yoshiki/mt/archives/000673.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; [2008-03-03]
;; anything.elを使うようにしたのでコメントアウト。

;; バッファの選択候補をインクリメンタルにしぼる
;; http://www.fan.gr.jp/~ring/Meadow/meadow.html#iswitchb.el
;; (require 'iswitchb)
;; (iswitchb-default-keybindings)

;; iswitchbで補完対象に含めないバッファ
;; (setq iswitchb-buffer-ignore
;;       '(
;;         "^ "
;;         "*Buffer*"
;;         "*PerlModules*"
;;         "Completions"
;;         ))

;; ミニバッファ内の入力を、インクリメンタルに選択
;; http://www.sodan.org/%7Eknagano/emacs/minibuf-isearch/
;; (require 'minibuf-isearch)
