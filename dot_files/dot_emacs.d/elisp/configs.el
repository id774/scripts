;; configs.el
;; 設定系

;;モードラインに今いる関数を表示
;;読み込み時のみスキャンっていうのがちょっといけてない
(which-func-mode)

;; blinkうざいし
(blink-cursor-mode nil)

;; ツールバー使う? 俺は使わんけど
(tool-bar-mode nil)

;; メニューバー使う?
(menu-bar-mode -1)

;; スクロールバー使う?
(scroll-bar-mode -1)

;; ホイールマウス使う? なら入れとけ。
(mouse-wheel-mode 1)

;; xtermとかgnome-terminalとか。
(xterm-mouse-mode 1)

;; ;; fringe(左右に余白のように見えてるアレ)
;; (fringe-mode 8)

;; ;; 時間表示
(display-time)

;; 行番号と列番号
(line-number-mode t)
(column-number-mode t)

;; 画像展開
(auto-image-file-mode)

;; interactive switch buffer
(iswitchb-mode)
(iswitchb-default-keybindings)

;; バックアップファイルの保存位置指定
;; CVSで管理していても設定しておくと安全
;; !path!to!file-name~ で保存される
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

;; transient-mark
(setq transient-mark-mode t)

;;isearch を色つきに
(setq search-highlight t)
(setq query-replace-highlight t)
;;(setq isearch-lazy-highlight-initial-delay 0) ; obsolate
(setq lazy-highlight-initial-delay 0)

;; M-x woman
(setq woman-manpath '("/usr/local/man"
		      "/usr/share/man"
		      "/usr/local/share/man"
		      "/sw/man"
		      "/usr/share/man/ja_JP.ujis"))
(setq woman-cache-filename (expand-file-name "~/.emacs.d/woman-cache"))

;; バックアップごときでinodeが変わるのが許せない
(setq backup-by-copying t)

;;GC間隔
(setq gc-cons-threshold 1000000)

;; スプラッシュ非表示 : 起動が速くなる
(setq inhibit-startup-message t)

;; ビープ音のかわりに画面反転
(setq visible-bell t)

;; あまりに大きいファイルは色付けると時間かかるので、上限を指定
(setq font-lock-maximum-size nil)

;; ;; fast-lock
;; (setq font-lock-support-mode 'fast-lock-mode)
;; (setq fast-lock-cache-directories '("~/.emacs.d/emacs-flc"))

;; auto-saveの場所
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")

;; 最後に改行を付ける。
(setq require-final-newline t)

;; /tmp でもまあいいんだけど。
;; (setq temporary-file-directory "~/.emacs.d/tmp")
(setq temporary-file-directory "/dev/shm")

;; 1行ずつスクロール。
(setq scroll-conservatively 1)

;;新規行を作成しない
;;emacs21ではデフォルト。
(setq next-line-add-newlines nil)

;; 80 だとちょっと……
(setq fill-column 79)

;; *Messages* の長さ
(setq message-log-max 200)

;; .gz なファイルとかを透過的に圧縮/伸張
(auto-compression-mode t)

;; apropos をあらゆるとことに
(setq apropos-do-all t)

;; abbrev
;; (read-abbrev-file "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)

;; version control
(setq vc-follow-symlinks t)
(setq vc-suppress-confirm t)
(setq vc-command-messages t)

;; narrowingするときにいちいち警告してくるのがウザイ
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; カーソル一個
(setq cursor-in-non-selected-windows nil)

;; 空行強調
(setq-default indicate-empty-lines t)

;; 行間(これ消してもいいかなぁ)
;; (setq-default line-spacing 0)

;; Anthy! Anthy!
;; (set-input-method "japanese-anthy")
;; (set-input-method "japanese-prime")


;; C言語系の設定群

;; Ruby default style
(c-add-style "ruby"
	     '("bsd"
	       (c-offsets-alist
		(case-label . 2)
		(label . 2)
		(statement-case-intro . 2))))

;; でも本当は stroustrup が好き。
(defun-add-hook 'c-mode-common-hook
  (c-set-style "Stroustrup")
  (c-toggle-hungry-state 1)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

;; fullscreen
(set-frame-parameter nil 'fullscreen 'fullboth)
(defun toggle-fullscreen ()
  (interactive)
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
      nil
      'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; Redo! (need byte-compile redo.el)
(when (require 'redo nil t)
  (define-key ctl-x-map (if window-system "U" "r") 'redo)
  (define-key global-map [?\C-.] 'redo))

;; キーバインド設定
(load "global-set-key")

;; カスタム設定
(load "custom")


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
