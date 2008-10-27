;; 行末の空白を一括削除する
;; usage M-x trim-buffer
(defun trim-buffer ()
  "Delete excess white space."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))
	  (goto-char (point-max))
    (delete-blank-lines)
	(mark-whole-buffer)
    (tabify (region-beginning) (region-end))))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
