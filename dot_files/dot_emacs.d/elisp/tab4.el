;; 4 タブにする(ローカルバッファのみ)
;; usage M-x tab4
;; fj.editor.muleより
(defun tab4 (arg)
  "Toggle `tab-width' between 4 and 8.
With arg, set `tab-width' to 4 if and only if arg is positive."
  (interactive "P")
  (let ((width (if (integerp arg)
		   (if (> arg 0) 4 8)
		 (if (eq 8 tab-width) 4 8))))
    (set (make-local-variable 'tab-width) width)
    (set (make-local-variable 'tab-stop-list) nil)
    (let ((n width)
	  (max 1024))
      (while (<= n max)
	(setq tab-stop-list (nconc tab-stop-list (list n))
	      n (+ n width))))
    (message "TAB=%d" width)))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:

