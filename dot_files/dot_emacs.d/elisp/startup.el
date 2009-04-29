;; startup.el
;; 環境設定してautoloads.elを呼ぶ

;; proxy
(load "~/.emacs.d/elisp/proxy")

;; chdir
(cd "~/")

;; path 系
(setq exec-path (append '("/usr/local/bin" "/sw/bin") exec-path))

(setenv "PATH" (concat '"/usr/local/bin:/sw/bin:/usr/bin:" (getenv "PATH")))

;;; my-load-path
;;; kaoru プロの設定を読んでかっこいいと思った。
(defvar default-load-path load-path
  "*Base of `load-path'.
It is used as a default value of target path to search file or
subdirectory under load-path.")
(setq my-load-path
      (list "/usr/local/share/emacs/site-lisp"
	    (expand-file-name "~/.emacs.d/elisp")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party/rhtml-mode")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party/haskell-mode")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party/fixed-width-fontset")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party/navi2ch")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party/italk")
	    (expand-file-name "~/.emacs.d/elisp/3rd-party/riece")))
(setq load-path (append my-load-path default-load-path))

;; custom
(setq custom-file (expand-file-name "~/.emacs.d/elisp/custom.el"))

;; whoami
(setq user-full-name "id774")
(setq user-mail-address "idnanashi@gmail.com")

;; main
(load "autoloads")


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
