;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 実行環境を判別する。
;; http://d.hatena.ne.jp/hito-d/20060220#1140445790

;; OSを判別
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (or (equal system-type 'usg-unix-v)
          (or  (equal system-type 'berkeley-unix)
               (equal system-type 'cygwin)))))

(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-system-v
  (equal system-type 'usg-unix-v))
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; cygwinもunixグループにしておく
  (equal system-type 'cygwin))

(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))

(defvar run-darwin (equal system-type 'darwin))

;; Emacsenの種類とヴァージョンを判別
(defvar run-emacs20
  (and (equal emacs-major-version 20)
       (null (featurep 'xemacs))))
(defvar run-emacs21
  (and (equal emacs-major-version 21)
       (null (featurep 'xemacs))))
(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs))))
(defvar run-meadow (featurep 'meadow))
(defvar run-meadow1 (and run-meadow run-emacs20))
(defvar run-meadow2 (and run-meadow run-emacs21))
(defvar run-meadow3 (and run-meadow run-emacs22))
(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))
(defvar run-carbon-emacs (and run-darwin window-system))

;; ユーティリティ関数

;; [2008-03-13]
;; add-to-load-path追加

;; 引数をload-pathへ追加する
(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
           (add-to-list 'load-path path))
        (mapcar 'expand-file-name paths)))

;; elispと設定ファイルのディレクトリをload-pathに追加
(add-to-load-path "~/.emacs.d/elisp"
                  "~/.emacs.d/conf")

;; eval-safe
;; 安全な評価。評価に失敗してもそこで止まらない。
;; http://www.sodan.org/~knagano/emacs/dotemacs.html#eval-safe
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

;; [2008-03-13]
;; clはどこで使ってるかわからんので、とりあえずrequireしとく。

(eval-when-compile
  (require 'cl))

;; 個別の設定をロードしまくりパート

;; 特定ディレクトリ以下を自動でロードするようにしてもいいけど、順番とか、
;; これやっぱ外しておこうとかいうのを調整するのが面倒。

(load "init-global")
(load "init-skk")
(load "init-window")
(load "init-keymaps")
(load "init-anything")
(load "init-woman")
(load "init-color")
(load "init-highlighting")
(load "init-minibuf")
(load "init-killring")
(load "init-abbrves")
(load "init-templates")
(load "init-shell")
(load "init-dired")
(load "init-migemo")
(load "init-sdic")
(load "init-w3m")
(load "init-languages")
(load "init-tags")
(load "init-vcs")
(load "init-html")
(load "init-css")
(load "init-javascript")
(load "init-c")
(load "init-perl")
(load "init-perlysense")
(load "init-ruby")
(load "init-php")
(load "init-lisp")
(load "init-haskell")
(load "init-scheme")
(load "init-yaml")
(load "init-hatena")
(load "init-autosave-buffers")
(load "init-elscreen")
(load "init-taskpaper")

;; [2008-03-13]
;; mmm-modeってば、なんか動かないんだよなー。

;; (load "init-mmm")

;; Meadow用設定を読み込む
(when (and run-w32 run-meadow)
  (load "init-meadow"))

;; Mac用設定を読み込む
(when run-darwin
  (load "init-mac"))
