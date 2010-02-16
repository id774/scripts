;; キーバインド設定

;; こっちのwidowで編集しながらあっちの*help*をスクロールとか。
(global-set-key "\M-V" 'scroll-other-window-down)

;; C-x p で C-x o の逆の動作をする
(define-key ctl-x-map "p"
  #'(lambda (arg) (interactive "p") (other-window (- arg))))

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

;; C-x t で行数表示/非表示
(cond
  ((>= emacs-major-version '23)
    (define-key global-map "\C-x\ t" 'linum-mode)))

;; C-x C-y または C-x y で auto-complete-mode の有効/無効を切り替える
(define-key global-map "\C-x\C-y" 'auto-complete-mode)
(define-key global-map "\C-x\ y" 'auto-complete-mode)

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
(setq windmove-wrap-around t)
(define-key global-map [(C shift n)] 'windmove-down)
(define-key global-map [(C shift p)] 'windmove-up)
(define-key global-map [(C shift b)] 'windmove-left)
(define-key global-map [(C shift f)] 'windmove-right)

;; ウィンドウ分割
(define-key global-map "\C-c\C-c\C-k" 'delete-window)
(define-key global-map "\C-c\C-c\ k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\C-y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\ y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\C-j" 'split-one-window)
(define-key global-map "\C-c\C-c\ j" 'split-window-horizontally)

;; 分割したウィンドウを時計回りに移動
(define-key global-map "\C-c\C-c\C-w" 'other-window)
(define-key global-map "\C-c\C-c\ w" 'other-window)
(define-key global-map "\C-c\C-c\C-c" 'other-window)
(define-key global-map "\C-c\C-c\ c" 'other-window)

;; Navi2ch
(defun switch-to-navi2ch()
  (interactive)
  (split-one-window-p)
  (navi2ch))
(define-key global-map "\C-c\C-c\C-i" 'switch-to-navi2ch)
(define-key global-map "\C-c\C-c\ i" 'switch-to-navi2ch)

;; 行末の空白を一括削除する
(define-key global-map "\C-c\C-c\ t" 'delete-trailing-whitespace)

;; タブ, 全角スペース、改行直前の半角スペースを表示する(トグルで動作)
(define-key global-map "\C-c\C-c\C-t" 'jaspace-mode)

;; C-M-g でも keyboard-escape-quit する
(global-set-key "\C-\M-g" 'keyboard-escape-quit)

;; C-x C-k でも kill bufferする
(define-key global-map "\C-x\C-k" 'kill-buffer)

;; C-h を backspace にする
(global-set-key "\C-h" 'delete-backward-char)

;; C-\ を help-command にする
(global-set-key "\C-\\" 'help-command)

;; C-x C-j または C-x j で view-mode を切り替える
(defun toggle-view-mode ()
  (interactive)
  (cond (view-mode
      (view-mode nil)
      (setq hl-line-mode nil))
    (t
      (view-mode))))
(define-key global-map "\C-x\C-j" 'toggle-view-mode)
(define-key global-map "\C-x\ j" 'toggle-view-mode)

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
(define-key global-map "\C-c\C-c\ b" 'buffer-menu)

;; 動的略語変換
(define-key global-map [C-return] 'dabbrev-expand)
(define-key global-map [C-S-return] 'dabbrev-completion)

;; C-x C-wを上書き保存にする(別名保存はC-x w)
(define-key global-map "\C-x\C-w" 'save-buffer)
(define-key global-map "\C-x\ w" 'write-file)

;; C-M-x C-wでも上書き保存する
(global-set-key "\C-\M-x\C-w" 'save-buffer)

;; C-x C-rの存在意義が無いのでライブラリ検索に割り当て
(global-set-key "\C-x\C-r" 'find-library)

;; バッファ先頭/末尾へのカーソル移動
(define-key global-map "\C-c\C-c\C-a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\ a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\C-e" 'end-of-buffer)
(define-key global-map "\C-c\C-c\ e" 'end-of-buffer)

;; Describe
(define-key global-map "\C-c\C-c\C-d" 'describe-variable)
(define-key global-map "\C-c\C-c\ d" 'describe-variable)
(define-key global-map "\C-c\C-c\C-f" 'describe-function)
(define-key global-map "\C-c\C-c\ f" 'describe-function)
(define-key global-map "\C-c\C-c\C-b" 'describe-bindings)

;; 検索/置換
(define-key global-map "\C-c\C-c\C-q" 'query-replace-regexp)
(define-key global-map "\C-c\C-c\ q" 'query-replace-regexp-eval)
(global-set-key "\C-x\C-q" 'query-replace)

;; grep
(define-key global-map "\C-c\C-c\C-g" 'grep-find)
(define-key global-map "\C-c\C-c\ g" 'grep)

;; ansi-term
(global-set-key "\C-x\C-a" '(lambda ()(interactive)(ansi-term "/bin/zsh")))
(global-set-key "\C-x\ a" '(lambda ()(interactive)(ansi-term "/bin/zsh")))

;; 矩形選択
(define-key global-map "\C-c\C-c\C-z" 'cua-mode)
(define-key global-map "\C-c\C-c\ z" 'cua-mode)

;; アンドゥ/リドゥ
(define-key global-map "\C-c\C-c\C-u" 'undo)
(define-key global-map "\C-c\C-c\ u" 'undo)
(define-key global-map "\C-c\C-c\C-r" 'redo)
(define-key global-map "\C-c\C-c\ r" 'redo)

;; バッファ再読み込み
(defun revert-current-buffer ()
  (interactive)
  (revert-buffer t t))
(defun revert-all-buffers ()
  (interactive)
  (let ((cbuf (current-buffer)))
    (dolist (buf (buffer-list))
      (if (not (buffer-file-name buf)) ;only the file which visit on path
   nil
 (switch-to-buffer buf)
 (revert-buffer t t)))
    (switch-to-buffer cbuf)
    ))
(define-key global-map "\C-c\C-c\C-p" 'revert-current-buffer)
(define-key global-map "\C-c\C-c\ p" 'revert-all-buffers)

;; 開いているすべてのバッファをkillする
(define-key global-map "\C-c\C-c\ 0" 'kill-all-buffers)

;; C-x C-cで必ず確認する
(defun confirm-save-buffers-kill-emacs ()
  (interactive)
  (if (yes-or-no-p "quit emacs? ")
    (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'confirm-save-buffers-kill-emacs)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
