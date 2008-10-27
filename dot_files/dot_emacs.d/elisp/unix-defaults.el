;; Unix default settings

;; 日本語設定
(set-language-environment 'Japanese)
;; 最近もう$LANGでいいやという気になった。
;; (set-default-coding-systems 'euc-jp-unix)
;; (set-buffer-file-coding-system 'euc-jp-unix)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'euc-jp-unix)
;; (setq file-name-coding-system 'euc-jp-unix)
;; (set-clipboard-coding-system 'iso-2022-jp-unix)
;; (setq default-process-coding-system '(undecided . euc-jp-unix))

;; 色設定
(if window-system
    (progn
      ;; M+
      (create-fontset-from-fontset-spec
       "-mplus-*-mplus-r-normal--10-*-*-*-*-*-fontset-mplus_10r,
       ascii:-mplus-gothic-medium-r-normal--10-*,
       japanese-jisx0208:-mplus-gothic-medium-r-normal--10-*-jisx0208.1990-0,
       katakana-jisx0201:-mplus-gothic-medium-r-normal--10-*-jisx0201.1976-0")
      (create-fontset-from-fontset-spec
       "-mplus-*-mplus-r-normal--12-*-*-*-*-*-fontset-mplus_10b,
       ascii:-mplus-gothic-bold-r-normal--10-*-iso8859-1,
       japanese-jisx0208:-mplus-gothic-bold-r-normal--10-*-jisx0208.1990-0,
       katakana-jisx0201:-mplus-gothic-bold-r-normal--10-*-jisx0201.1976-0")
      (create-fontset-from-fontset-spec
       "-mplus-*-mplus-r-normal--12-*-*-*-*-*-fontset-mplus_12r,
       ascii:-mplus-gothic-medium-r-normal--12-*,
       japanese-jisx0208:-mplus-gothic-medium-r-normal--12-*-jisx0208.1990-0,
       katakana-jisx0201:-mplus-gothic-medium-r-normal--12-*-jisx0201.1976-0")
      (create-fontset-from-fontset-spec
       "-mplus-*-mplus-r-normal--12-*-*-*-*-*-fontset-mplus_12b,
       ascii:-mplus-gothic-bold-r-normal--12-*,
       japanese-jisx0208:-mplus-gothic-bold-r-normal--12-*-jisx0208.1990-0,
       katakana-jisx0201:-mplus-gothic-bold-r-normal--12-*-jisx0201.1976-0")
      (set-default-font "fontset-mplus_12r")
      (if t
	  (progn
	    (setq default-frame-alist
		  (append
		   (list '(foreground-color . "alice blue")
			 '(background-color . "black")
			 '(border-color . "white")
			 '(cursor-color . "azure")
			 '(mouse-color . "orange")
			 '(vertical-scroll-bars . nil)
			 '(font . "fontset-mplus_12r"))
		   default-frame-alist))
	    (set-face-foreground 'modeline "black")
	    (set-face-background 'modeline "gray80"))
	(progn
	  (setq default-frame-alist
		(list '(foreground-color . "black")
		      '(background-color . "snow")
		      '(border-color . "black")
		      '(cursor-color . "MidnightBlue")
		      '(mouse-color . "orange")
		      '(width . 80)
		      '(height . 40)
		      '(top . 0)
		      '(left . 0)
		      '(vertical-scroll-bars . nil))
		default-frame-alist)
	  (set-face-foreground 'modeline "alice blue")
	  (set-face-background 'modeline "black")))))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
