;; key-chord.el専用キーバインド設定

;; yu で auto-complete-modeの有効/無効を切り替える
(key-chord-define-global "yu" 'auto-complete-mode)

;; ウィンドウ移動
(key-chord-define-global "io" 'windmove-up)
(key-chord-define-global ",." 'windmove-down)
(key-chord-define-global "hj" 'windmove-left)
(key-chord-define-global ";:" 'windmove-right)

;; fg で keyboard-escape-quit する
(key-chord-define-global "fg" 'keyboard-escape-quit)

;; jk で view-mode を切り替える
(key-chord-define-global "jk" 'toggle-view-mode)

;; バッファ切り替え
(key-chord-define-global "m," 'previous-buffer)
(key-chord-define-global "ui" 'backward-buffer)

;; バッファリスト
(key-chord-define-global "kl" 'electric-buffer-list)

;; バッファ先頭/末尾へのカーソル移動
(key-chord-define-global "rt" 'beginning-of-buffer)
(key-chord-define-global "vb" 'end-of-buffer)

;; アンドゥ/リドゥ
(key-chord-define-global "l;" 'undo)
(key-chord-define-global "op" 'redo)

;; ファイルを開く
(key-chord-define-global "df" 'find-file)

;; スクロール
(key-chord-define-global "er" 'scroll-up)
(key-chord-define-global "cv" 'scroll-down)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
