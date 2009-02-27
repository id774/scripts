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

;; ウィンドウが1つしかない場合は縦に分割する関数
(defun split-one-window-p ()
  (if (one-window-p)
    (split-window-horizontally)))
(defun split-one-window ()
  (interactive)
  (split-one-window-p))

;; ウィンドウ移動
(global-set-key [right] 'windmove-right)
(global-set-key [left] 'windmove-left)
(define-key global-map [up] 'windmove-up)
(define-key global-map [down] 'windmove-down)

;; ウィンドウ分割
(define-key global-map "\C-c\C-c\C-k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\ k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\C-y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\ y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\C-j" 'split-one-window)
(define-key global-map "\C-c\C-c\ j" 'split-one-window)

;; 分割したウィンドウを時計回りに移動
(define-key global-map "\C-c\C-c\C-w" 'other-window)
(define-key global-map "\C-c\C-c\ w" 'other-window)
(define-key global-map "\C-c\C-c\C-c" 'other-window)
(define-key global-map "\C-c\C-c\ c" 'other-window)

;; Twitter
(defun switch-to-twitter1-mode ()
  (interactive)
  (twitter1-mode))
(defun switch-to-twitter2-mode ()
  (interactive)
  (twitter2-mode))
(defun switch-to-twitter3-mode ()
  (interactive)
  (twitter3-mode))
(defun switch-to-twitter4-mode ()
  (interactive)
  (twitter4-mode))
(defun switch-to-twitter-all-mode-left ()
  (interactive)
  (split-one-window-p)
  (split-window-vertically)
  (twitter1-mode)
  (split-window-vertically)
  (windmove-down)
  (twitter2-mode)
  (windmove-down)
  (twitter3-mode)
  (split-window-vertically)
  (windmove-down)
  (twitter4-mode))
(defun switch-to-twitter-all-mode-up ()
  (interactive)
  (split-window-vertically)
  (twitter1-mode)
  (split-window-vertically)
  (split-window-horizontally)
  (windmove-right)
  (twitter2-mode)
  (windmove-left)
  (windmove-down)
  (twitter3-mode)
  (split-window-horizontally)
  (windmove-right)
  (twitter4-mode)
  (windmove-down))
(define-key global-map "\C-c\C-c\ 1" 'switch-to-twitter1-mode)
(define-key global-map "\C-c\C-c\ 2" 'switch-to-twitter2-mode)
(define-key global-map "\C-c\C-c\ 3" 'switch-to-twitter3-mode)
(define-key global-map "\C-c\C-c\ 4" 'switch-to-twitter4-mode)
(define-key global-map "\C-c\C-c\ 5" 'switch-to-twitter-all-mode-left)
(define-key global-map "\C-c\C-c\ 6" 'switch-to-twitter-all-mode-up)

;; Navi2ch
(defun switch-to-navi2ch()
  (interactive)
  (split-one-window-p)
  (navi2ch))
(define-key global-map "\C-c\C-c\C-i" 'switch-to-navi2ch)
(define-key global-map "\C-c\C-c\ i" 'switch-to-navi2ch)

;; C-M-g でも keyboard-escape-quit する
(global-set-key "\C-\M-g" 'keyboard-escape-quit)

;; C-x C-k でも kill bufferする
(define-key global-map "\C-x\C-k" 'kill-buffer)

;; C-h を backspace にする
(global-set-key "\C-h" 'delete-backward-char)

;; C-x C-y または C-x y で view-mode を切り替える
(defun toggle-view-mode ()
  (interactive)
  (cond (view-mode
      (view-mode nil)
      (setq hl-line-mode nil))
    (t
      (view-mode))))
(define-key global-map "\C-x\C-y" 'toggle-view-mode)
(define-key global-map "\C-x\ y" 'toggle-view-mode)

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

;; C-x C-wを上書き保存にする(別名保存はC-x w)
(define-key global-map "\C-x\C-w" 'save-buffer)
(define-key global-map "\C-x\ w" 'write-file)

;; C-M-x C-wでも上書き保存する
(global-set-key "\C-\M-x\C-w" 'save-buffer)

;; バッファ先頭/末尾へのカーソル移動
(define-key global-map "\C-c\C-c\C-a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\ a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\C-e" 'end-of-buffer)
(define-key global-map "\C-c\C-c\ e" 'end-of-buffer)

;; アンドゥ/リドゥ
(define-key global-map "\C-c\C-c\C-u" 'undo)
(define-key global-map "\C-c\C-c\ u" 'undo)
(define-key global-map "\C-c\C-c\C-r" 'redo)
(define-key global-map "\C-c\C-c\ r" 'redo)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
