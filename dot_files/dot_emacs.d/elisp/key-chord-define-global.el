;; key-chord.el専用キーバインド設定

;; yu で auto-complete-modeの有効/無効を切り替える
(key-chord-define-global "yu" 'auto-complete-mode)

;; ウィンドウ移動
(key-chord-define-global "io" 'windmove-up)
(key-chord-define-global ",." 'windmove-down)
(key-chord-define-global "hj" 'windmove-left)
(key-chord-define-global "l;" 'windmove-right)

;; fg で keyboard-escape-quit する
(key-chord-define-global "fg" 'keyboard-escape-quit)

;; jk で view-mode を切り替える
(key-chord-define-global "jk" 'toggle-view-mode)

;; バッファ切り替え
(key-chord-define-global "m," 'previous-buffer)
(key-chord-define-global "ui" 'backward-buffer)

;; バッファ先頭/末尾へのカーソル移動
(key-chord-define-global "rt" 'beginning-of-buffer)
(key-chord-define-global "vb" 'end-of-buffer)

;; スクロール
(key-chord-define-global "er" 'scroll-down)
(key-chord-define-global "cv" 'scroll-up)

;; バッファリスト
(key-chord-define-global "kl" 'electric-buffer-list)
(key-chord-define-global "nm" 'iswitchb-buffer)
(key-chord-define-global "bn" 'buffer-menu)

;; ファイルを開く
(key-chord-define-global "df" 'find-file)

;; Anthing.el
(key-chord-define-global "as" 'anything)
(key-chord-define-global "sd" 'anything)
(key-chord-define-global ";:" 'anything)

;; 矩形選択
(key-chord-define-global "we" 'cua-mode)

;; アンドゥ/リドゥ
(key-chord-define-global "zx" 'undo)
(key-chord-define-global "qw" 'redo)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
