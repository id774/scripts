;; *scratch* がきえないようにする
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#scratch
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
		   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
	  ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
	  (lambda ()
	    (if (string= "*scratch*" (buffer-name))
		(progn (my-make-scratch 0) nil)
	      t)))
(add-hook 'after-save-hook
	  (lambda ()
	    (unless (member (get-buffer "*scratch*") (buffer-list))
	      (my-make-scratch 1))))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
