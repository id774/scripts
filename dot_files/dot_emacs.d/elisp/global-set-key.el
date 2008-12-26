;; キーバインド設定

;; こっちのwidowで編集しながらあっちの*help*をスクロールとか。
(global-set-key "\M-V" 'scroll-other-window-down)

;; 普段、インデントするようにする
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; No more bobcat, no more keyswap!
(cond ((eq window-system 'x)
       (progn
	 (global-set-key [delete] 'delete-char)))
      ((eq window-system 'mac)
       t) ;; ok
      (t (keyboard-translate ?\C-h ?\C-?)))

;; auto-complete-modeの有効/無効を切り替える
(define-key global-map "\C-x\C-a" 'auto-complete-mode)
(define-key global-map "\C-c\C-c\ c" 'auto-complete-mode)

;; Twitter
(define-key global-map "\C-c\C-c\ 1" 'twitter1-mode)
(define-key global-map "\C-c\C-c\ 2" 'twitter2-mode)
(define-key global-map "\C-c\C-c\ 3" 'twitter3-mode)
(define-key global-map "\C-c\C-c\ 4" 'twitter4-mode)

;; C-M-g でも keyboard-escape-quit する
(global-set-key "\C-\M-g" 'keyboard-escape-quit)

;; C-x C-k でも kill bufferする
(define-key global-map "\C-x\C-k" 'kill-buffer)

;; C-h を backspace にする
(global-set-key "\C-h" 'delete-backward-char)

;; ウィンドウ移動
(global-set-key [right] 'windmove-right)
(global-set-key [left] 'windmove-left)
(define-key global-map [up] 'windmove-up)
(define-key global-map [down] 'windmove-down)

;; ウィンドウ分割
(define-key global-map "\C-c\C-c\ k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\ y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\ j" 'split-window-horizontally)

;; 分割したウィンドウを時計回りに移動
(define-key global-map "\C-c\C-c\ w" 'other-window)

;; C-x C-y または C-x y で view-mode を切り替える
(defun edit-mode ()
  (interactive)
  (cond (view-mode
      (view-mode nil)
      (setq hl-line-mode nil))
    (t
      (view-mode))))
(define-key global-map "\C-x\C-y" 'edit-mode)
(define-key global-map "\C-x\ y" 'edit-mode)

;; バッファをM-n,M-pで切り替え
(defun previous-buffer ()
  "Select previous window."
  (interactive)
  (bury-buffer))
(defun backward-buffer ()
  "Select backward window."
  (interactive)
  (switch-to-buffer
    (car (reverse (buffer-list)))))
(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\M-p" 'backward-buffer)

;; バッファリスト
(define-key global-map "\C-x\C-b" 'electric-buffer-list)
(define-key global-map "\C-c\C-c\C-c" 'electric-buffer-list)

;; C-x C-wを上書き保存にする(別名保存はC-x w)
(define-key global-map "\C-x\C-w" 'save-buffer)
(define-key global-map "\C-x\ w" 'write-file)

;; C-M-x C-wでも上書き保存する
(global-set-key "\C-\M-x\C-w" 'save-buffer)

;; バッファ先頭/末尾へのカーソル移動
(define-key global-map "\C-c\C-c\ a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\ e" 'end-of-buffer)

;; アンドゥ/リドゥ
(define-key global-map "\C-c\C-c\ u" 'undo)
(define-key global-map "\C-c\C-c\ r" 'redo)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
