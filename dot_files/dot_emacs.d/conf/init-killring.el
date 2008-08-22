;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; kill-summary C-yした後にM-yすると、候補を表示するウィンドウが現れる
;; http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/elisp/kill-summary.el
;;
;; [使い方]
;;   p/n(or j/k)で上下移動しながらkill-ringから文字列を置き換える
;;   C-p/C-nだと、候補を移動するだけ
;;   SPC で現在行を選択．C-v (scroll-up) でスクロール
;;   d で現在行のキルリングを即座に消去

(autoload 'kill-summary "kill-summary" nil t)
(global-set-key "\M-y" 'kill-summary)
