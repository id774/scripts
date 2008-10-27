;;存在しないファイルを開こうとしたとき念押し
;;fj.editor.emacsより
(add-hook 'find-file-not-found-hooks 'new-file-p)
(defun new-file-p ()
  (interactive)
  (or (y-or-n-p
       (format "\"%s\"not found. Create this file?"
	       buffer-file-name))))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
