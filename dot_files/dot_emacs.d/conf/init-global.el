;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 文字コードの変更方法
;;
;; キーボードから入力する文字コード
;; C-x RET kの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x set-keyboard-coding-systemと同じ
;;
;; 画面表示に使用する文字コード
;; C-x RET tの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x set-terminal-coding-systemと同じ
;;
;; ファイルの保存に使用する文字コード(カレントバッファのみ)
;; C-x RET fの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x set-buffer-file-coding-systemと同じ
;;
;; バッファやファイルの文字コード(emacs全体で有効)
;; C-x RET cの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x universal-coding-system-argumentと同じ
;;
;; 文字コードを指定して再読み込み
;; C-x RET rの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x revert-buffer-with-coding-systemと同じ

;; apel, emuをロードパスに追加
;; w3mかなんかで使う。
(add-to-load-path "~/.emacs.d/elisp/emu"
                  "~/.emacs.d/elisp/apel")

;; [2008-03-13]
;; これ、必要なの？

;; exec-path, PATHの追加
(let ((path (list "~/bin" "~/local/bin" "/usr/local/bin" "/usr/local/sbin" "/opt/local/bin" "/usr/bin" "/usr/sbin")))
  (setq exec-path (append path exec-path))
  (setenv "PATH" (concat (mapconcat 'identity path ":") ":" (getenv "PATH"))))

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; emacs-serverを起動(emacsclientで使用)
(server-start)

;; エンコーディングは基本的にUTF-8
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; メニューバー、ツールバー非表示
(eval-safe (menu-bar-mode nil))
(eval-safe (tool-bar-mode nil))

;; ヴィジブルベルを抑制
(setq visible-bell nil)

;; ビープ音を抑制
(setq ring-bell-function '(lambda ()))

;; カーソルの点滅を抑制
(blink-cursor-mode 0)

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存したファイルを削除する。
(setq delete-auto-save-files t)

;; 自動セーブしない。
(setq auto-save-default nil)

;; yes/noを、y/nで選択できるようにする。
(fset 'yes-or-no-p 'y-or-n-p)

;; kill-lineで行末の改行文字も削除
(setq kill-whole-line t)

;; リージョンをC-hで削除
(delete-selection-mode 1)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)

;; マクロ読み込み
(load (expand-file-name"~/.emacs.d/conf/macros"))

;; 個人用infoディレクトリを追加
(setq Info-default-directory-list
      (cons (expand-file-name "~/.emacs.d/info/") Info-default-directory-list))

;; 記号やその他の文字等を正しくマッピングする
;; Emacs辞典p.395
;; http://www.pqrs.org/~tekezo/emacs/doc/wide-character/index.html
(utf-translate-cjk-set-unicode-range
 '((#x00a2 . #x00a3)
   (#x00a7 . #x00a8)
   (#x00ac . #x00ac)
   (#x00b0 . #x00b1)
   (#x00b4 . #x00b4)
   (#x00b6 . #x00b6)
   (#x00d7 . #x00d7)
   (#X00f7 . #x00f7)
   (#x0370 . #x03ff)
   (#x0400 . #x04FF)
   (#x2000 . #x206F)
   (#x2100 . #x214F)
   (#x2190 . #x21FF)
   (#x2200 . #x22FF)
   (#x2300 . #x23FF)
   (#x2500 . #x257F)
   (#x25A0 . #x25FF)
   (#x2600 . #x26FF)
   (#x2e80 . #xd7a3)
   (#xff00 . #xffef)))

;; バッファ全部を自動的に保存・復元
;; http://www.hasta-pronto.org/archives/2008/01/30-0235.php
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe emacs" t)
(define-key ctl-x-map "R" 'resume)                        ; C-x R で復元
(define-key ctl-x-map "K" 'wipe)                          ; C-x K で Kill
(add-hook 'kill-emacs-hook 'save-current-configuration)   ; 終了時に保存
