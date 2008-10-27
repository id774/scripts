;; utils.el
;; .emacs の中で使う関数

;; http://www.sodan.org/~knagano/emacs/dotemacs.html
;; とか参照。

(defun autoload-p (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))
(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook のエイリアス。引数を関数にパックして hook に追加する。"
  `(add-hook ,hookname
	     (function (lambda () ,@sexplist))))
(defun load-p (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
	(message (format "failed to load %s" loadlib)))
    load-status))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
