;; $B%-!<%P%$%s%I@_Dj(B

;; $B$3$C$A$N(Bwidow$B$GJT=8$7$J$,$i$"$C$A$N(B*help*$B$r%9%/%m!<%k$H$+!#(B
(global-set-key "\M-V" 'scroll-other-window-down)

;; $BIaCJ!"%$%s%G%s%H$9$k$h$&$K$9$k(B
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; No more bobcat, no more keyswap!
(cond ((eq window-system 'x)
       (progn
	 (global-set-key [delete] 'delete-char)))
      ((eq window-system 'mac)
       t) ;; ok
      (t (keyboard-translate ?\C-h ?\C-?)))

;; auto-complete-mode$B$NM-8z(B/$BL58z$r@Z$jBX$($k(B
(define-key global-map "\C-x\C-a" 'auto-complete-mode)

;; $B%&%#%s%I%&$,(B1$B$D$7$+$J$$>l9g$O=D$KJ,3d$9$k4X?t(B
(defun split-one-window-p ()
  (if (one-window-p)
    (split-window-horizontally)))
(defun split-one-window ()
  (interactive)
  (split-one-window-p))

;; $B%&%#%s%I%&0\F0(B
(global-set-key [right] 'windmove-right)
(global-set-key [left] 'windmove-left)
(define-key global-map [up] 'windmove-up)
(define-key global-map [down] 'windmove-down)

;; $B%&%#%s%I%&J,3d(B
(define-key global-map "\C-c\C-c\C-k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\ k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\C-y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\ y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\C-j" 'split-one-window)
(define-key global-map "\C-c\C-c\ j" 'split-one-window)

;; $BJ,3d$7$?%&%#%s%I%&$r;~7W2s$j$K0\F0(B
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

;; C-M-g $B$G$b(B keyboard-escape-quit $B$9$k(B
(global-set-key "\C-\M-g" 'keyboard-escape-quit)

;; C-x C-k $B$G$b(B kill buffer$B$9$k(B
(define-key global-map "\C-x\C-k" 'kill-buffer)

;; C-h $B$r(B backspace $B$K$9$k(B
(global-set-key "\C-h" 'delete-backward-char)

;; C-x C-y $B$^$?$O(B C-x y $B$G(B view-mode $B$r@Z$jBX$($k(B
(defun toggle-view-mode ()
  (interactive)
  (cond (view-mode
      (view-mode nil)
      (setq hl-line-mode nil))
    (t
      (view-mode))))
(define-key global-map "\C-x\C-y" 'toggle-view-mode)
(define-key global-map "\C-x\ y" 'toggle-view-mode)
(define-key global-map [C-backspace] 'toggle-view-mode)

;; $B%P%C%U%!$r(BM-n,M-p$B$G@Z$jBX$((B
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

;; $B%P%C%U%!%j%9%H(B
(define-key global-map "\C-x\C-b" 'electric-buffer-list)
(define-key global-map [C-return] 'electric-buffer-list)

;; C-x C-w$B$r>e=q$-J]B8$K$9$k(B($BJLL>J]B8$O(BC-x w)
(define-key global-map "\C-x\C-w" 'save-buffer)
(define-key global-map "\C-x\ w" 'write-file)

;; C-M-x C-w$B$G$b>e=q$-J]B8$9$k(B
(global-set-key "\C-\M-x\C-w" 'save-buffer)

;; $B%P%C%U%!@hF,(B/$BKvHx$X$N%+!<%=%k0\F0(B
(define-key global-map "\C-c\C-c\C-a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\ a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\C-e" 'end-of-buffer)
(define-key global-map "\C-c\C-c\ e" 'end-of-buffer)

;; $B%"%s%I%%(B/$B%j%I%%(B
(define-key global-map "\C-c\C-c\C-u" 'undo)
(define-key global-map "\C-c\C-c\ u" 'undo)
(define-key global-map "\C-c\C-c\C-r" 'redo)
(define-key global-map "\C-c\C-c\ r" 'redo)

;; C-x C-j$B$G%P%C%U%!:FFI$_9~$_(B
(define-key global-map "\C-x\C-j" 'revert-buffer)
(define-key global-map "\C-x\ j" 'revert-buffer)

;; C-x C-c$B$GI,$:3NG'$9$k(B
(defun confirm-save-buffers-kill-emacs ()
  (interactive)
  (if (yes-or-no-p "quit emacs? ")
    (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'confirm-save-buffers-kill-emacs)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
