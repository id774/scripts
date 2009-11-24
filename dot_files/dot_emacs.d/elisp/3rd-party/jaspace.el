;;; -*- coding: shift_jis-dos; tab-width: 4; -*-
;;; jaspace.el --- make Japanese whitespaces visible
;;; $Id: jaspace.el 1.1 2005/02/26 06:21:02 satomii Exp $

;; Copyright (C) 2002-2004, Satomi I.
;; (satomi atmark ring period gr period jp)

;; This file is NOT a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; `jaspace-mode' makes Japanese 2-byte whitespaces visible by taking
;; advantage of buffer-display-table and font-lock features. It can also
;; show newlines (and experimentally tab characters) with the alternate
;; strings.
;;
;; This code has been tested on GNU Emacs 21.[23] (msvc-nt) only. Emacs
;; prior to version 21.x and XEmacs are not supported.

;;; Install:

;; Just add the following line to your .emacs somewhere after fundamental
;; features, especially coding systems, have been set.
;;
;;   (require 'jaspace)
;;
;; jaspace-mode will be automatically turned on for the buffers whose
;; major mode is contained in `jaspace-modes'. To manually (de)activate
;; it, use the command jaspace-mode(, -on, -off).

;;; Customization:

;; To control mode-based automatic activation:
;;
;;   (setq jaspace-modes nil)         ; disable automatic activation
;;   (setq jaspace-mdoes '(cc-mode))  ; activate on cc-mode only
;;
;; To change the alternate string for a Japanese space character:
;;
;;   (setq jaspace-alternate-jaspace-string "__")  ; or any other string
;;
;; To enable end-of-line marker:
;;
;;   (setq jaspace-alternate-eol-string "\xab\n")  ; or any other string
;;
;; To enable tab marker:
;;
;;   (setq jaspace-highlight-tabs t)  ; highlight tabs
;;
;; EXPERIMENTAL: On Emacs 21.3.50.1 (as of June 2004) or 22.0.5.1, a tab
;; character may also be shown as the alternate character if
;; font-lock-mode is enabled.
;;
;;   (setq jaspace-highlight-tabs ?^) ; use ^ as a tab marker
;;
;; Use M-x customize-group jaspace RET for further customization and/or
;; changing face attributes.

;;; Additional Notes:

;; jaspace-mode is not compatible with mmm-mode by default. If you want
;; to use both jaspace-mode and mmm-mode, try:
;;
;;     (add-hook 'mmm-mode-hook 'jaspace-mmm-mode-hook)

;;; Code:

(eval-and-compile
  (if (featurep 'xemacs)
	  (error "jaspace-mode does not work on XEmacs"))
  (if (< emacs-major-version 21)
	  (message "jaspace-mode may not work properly on this system")))

(require 'font-lock)
(require 'timer)
(require 'advice)

(condition-case nil
	(require 'whitespace)
  (error nil))

;; * customizable variables

(defgroup jaspace nil
  "Minor mode to make Japanese whitespaces visible."
  :group 'convenience)

(defcustom jaspace-alternate-jaspace-string
  (if (<= 21 emacs-major-version) " ")
  "Alternate string to indicate a Japanese whitespace."
  :type '(choice (const nil) string)
  :group 'jaspace)

(defcustom jaspace-alternate-eol-string nil
  "Alternate string to be used as an end-of-line marker.
Be sure to append a `\n' at the end of the string when you customize
this value.

NOTE: Do not set this variable on Emacs 20 or ealier. It may cause all
lines on the buffer to be concatenated."
  :type '(choice (const nil) string)
  :group 'jaspace)

(defcustom jaspace-highlight-tabs nil
  "Non-nil means jaspace-mode also displays and/or highlights tab
characters when font-lock mode is enabled.

If a character, it is used as a tab marker displayed at each tab
position. Note that this is an experimental feature that would work on
Emacs 21.3.50.1 (or maybe later) only."
  :type '(choice (const nil)
				 (const t)
				 (character :value ?^))
  :group 'jaspace)

(defface jaspace-highlight-jaspace-face
  (if (null jaspace-alternate-jaspace-string)
	  '((t (:background "rosybrown")))
	'((((class color) (background light)) (:foreground "azure3"))
	  (((class color) (background dark)) (:foreground "pink4"))))
  "Face used to highlight Japanese whitespaces when font-lock mode is on."
  :group 'jaspace)

(defface jaspace-highlight-eol-face
  '((((class color) (background light)) (:foreground "darkseagreen"))
	(((class color) (background dark)) (:foreground "darkcyan")))
  "Face used to highlight end-of-line markers when font-lock mode is on.
Used only when `jaspace-alternate-eol-string' is non-nil."
  :group 'jaspace)

(defface jaspace-highlight-tab-face
  (cond ((integerp jaspace-highlight-tabs)
		 '((((class color) (background light)) (:foreground "gray70"))
		   (((class color) (background dark)) (:foreground "gray30"))))
		((<= 21 emacs-major-version)
		 '((((class color) (background light))
			(:strike-through t :foreground "gray80"))
		   (((class color) (background dark))
			(:strike-through t :foreground "gray20"))))
		(t
		 '((((class color) (background light)) (:background "gray90"))
		   (((class color) (background dark)) (:background "gray10")))))
  "Face used to highlight tab characters when font-lock mode is on and
`jaspace-highlight-tabs' is non-nil."
  :group 'jaspace)

(defcustom jaspace-mode-string " JaSp"
  "String displayed on the mode-line to when jaspase-mode is on."
  :group 'jaspace
  :type 'string)

(defcustom jaspace-follow-font-lock t
  "Non-nil means jaspace-mode follows the font-lock mode, i.e., will be
automatically turned off when font-lock mode is disabled."
  :group 'jaspace
  :type 'boolean)

(defcustom jaspace-disable-on-read-only-buffers nil
  "Non-nil means disable jaspace-mode when the buffer is or has become
read-only."
  :group 'jaspace
  :type 'boolean)

(defcustom jaspace-idle-delay
  (if (boundp 'idle-update-delay) idle-update-delay 1)
  "Delay time in seconds before updating the status of jaspace-mode."
  :group 'jaspace
  :type 'number)

(defcustom jaspace-mode-hook nil
  "Hook to run when jaspace-mode is activated."
   :group 'jaspace
   :type 'hook)

(defcustom jaspace-mode-off-hook nil
  "Hook to run when jaspace-mode is deactivated."
  :group 'jaspace
  :type 'hook)

(defcustom jaspace-modes
  (if (boundp 'whitespace-modes)
	  (append whitespace-modes (list 'lisp-interaction-mode))
	'(asm-mode awk-mode autoconf-mode c-mode c++-mode cc-mode change-log-mode
	  cperl-mode emacs-lisp-mode java-mode html-mode lisp-mode
	  lisp-interaction-mode m4-mode makefile-mode objc-mode pascal-mode
	  perl-mode sh-mode shell-script-mode sgml-mode xml-mode))
  "List of major mode symbols to enable jaspace-mode automatically."
  :group 'jaspace
  :type '(repeat (symbol :tag "Major Mode")))

;; * non-customizable variables

(defvar jaspace-mode nil
  "Non-nil means jaspace-mode is currently enabled.
Setting this variable directly does not take effect; use the command
`jaspace-mode', `jaspace-mode-on' or `jaspace-mode-off' instead.")
(make-variable-buffer-local 'jaspace-mode)

(defvar jaspace-base-mode nil)

(defvar jaspace-original-display-table nil
  "Buffer's original display table.")
(defvar jaspace-font-lock-keywords nil
  "List of font lock keywords managed by jaspace-mode.")
(defvar jaspace-tab-width nil
  "Current tab width used to make alternative tab strings.")
(defvar jaspace-idle-timer nil
  "Timer used to update the status of jaspace-mode.")
(defvar jaspace-inhibit-update nil
  "Non-nil if jaspace-mode should not update its status.")

(defconst jaspace-advice-functions
  (append
   ;; not necessary when both `jaspace-alternate-jaspace-string' and
   ;; `jaspace-alternate-eol-string' are nil, though.
   '(char-width string-width)
   (unless (or (string= emacs-version "21.4.1")
			   (string-lessp emacs-version "21.3.50.1"))
	 '(vertical-motion))))

;; * functions

(defmacro jaspace-fontify-buffer ()
  '(if font-lock-mode
	   (font-lock-fontify-buffer)))

;; Emacs 20.7 does not have font-lock-remove-keywords.
(if (fboundp 'font-lock-remove-keywords)
	(defun jaspace-font-lock-remove-keywords ()
	  (font-lock-remove-keywords nil jaspace-font-lock-keywords))
  (defun jaspace-font-lock-remove-keywords ()
	(setcdr font-lock-keywords
			(delq nil
				  (mapcar (lambda (keyword)
							(unless (memq keyword jaspace-font-lock-keywords)
							  keyword))
						  (cdr font-lock-keywords))))))

(defun jaspace-set-idle-timer ()
  (if jaspace-idle-timer
	  (timer-set-idle-time jaspace-idle-timer jaspace-idle-delay t)
	(setq jaspace-idle-timer
		  (run-with-idle-timer jaspace-idle-delay t 'jaspace-update))))

(defun jaspace-font-lock-keywords ()
  "Return a list of font lock keywords used by jaspace-mode."
  (let ((keywords '(("@" (0 'jaspace-highlight-jaspace-face prepend)))))
	(if (and jaspace-alternate-eol-string
			 (string-lessp "" jaspace-alternate-eol-string))
		(setq keywords (cons '("\n" (0 'jaspace-highlight-eol-face prepend))
							 keywords)))
	(when jaspace-highlight-tabs
	  (let ((face (if (not (local-variable-p 'jaspace-tab-width))
					  (list 'quote 'jaspace-highlight-tab-face)
					;; probably the following form would be valid only
					;; when `font-lock-extra-managed-props' is available.
					`(let ((width (save-excursion
									(goto-char (match-beginning 0))
									(- jaspace-tab-width
									   (% (current-column)
										  jaspace-tab-width)))))
					   (list 'face 'jaspace-highlight-tab-face
							 'display (concat ,(char-to-string
												jaspace-highlight-tabs)
											  (make-string (1- width) ?\ ))
							 'jaspace-alt t)))))
		(setq keywords (cons (list "\t" (list 0 face 'prepend)) keywords))
		(setq keywords (cons '("[\t ]+$"
							   (0
								(if (and (boundp 'show-trailing-whitespace)
										 show-trailing-whitespace)
									'trailing-whitespace
								  'default)
								append))
							 keywords))))
	(nreverse keywords)))

(defun jaspace-active-count ()
  "Return the number of buffers on which jaspace-mode is active."
  (length (delq nil
				(mapcar (lambda (buffer)
						  ;; `buffer-local-value' is not available on
						  ;; earlier than Emacs 21.3.50.1.
						  (cdr (assq 'jaspace-mode
									 (buffer-local-variables buffer))))
						(buffer-list)))))

(defun jaspace-mode-enter ()
  (let ((jaspace-inhibit-update t)
		(jaspace-alternate-jaspace-string jaspace-alternate-jaspace-string)
		(jaspace-alternate-eol-string jaspace-alternate-eol-string))
	(if (and jaspace-alternate-jaspace-string
			 (string= "" jaspace-alternate-jaspace-string))
		(setq jaspace-alternate-jaspace-string nil))
	(if (and jaspace-alternate-eol-string
			 (string= "" jaspace-alternate-eol-string))
		(setq jaspace-alternate-eol-string nil))
	;; change the buffer's display table.
	(when (or jaspace-alternate-jaspace-string
			  jaspace-alternate-eol-string)
	  (or (local-variable-p 'jaspace-original-display-table)
		  (set (make-local-variable 'jaspace-original-display-table)
			   (copy-sequence buffer-display-table)))
	  (setq buffer-display-table (or buffer-display-table
									 (copy-sequence standard-display-table)
									 (make-display-table)))
	  ;; make Japanese whitespace visible.
	  (if jaspace-alternate-jaspace-string
		  (aset buffer-display-table ?@
				(vconcat jaspace-alternate-jaspace-string)))
	  ;; set end-of-line marker. this may cause a problem regarding
	  ;; vertical motion on Emacs 21.3.50.1 (as of Jan 2005) if
	  ;; `line-move-ignore-invisible' is non-nil. using display text
	  ;; property as a workaround is not applicable, since it causes
	  ;; another problem that empty lines are all hidden. the only
	  ;; working workaround is either to advice `vertical-motion' or
	  ;; to set `line-move-ignore-invisible' to nil.
	  (if jaspace-alternate-eol-string
		  (aset buffer-display-table ?\n
				(vconcat jaspace-alternate-eol-string))))
	;; remember the current tab width if necessary. this should be
	;; done before calling `jaspace-font-lock-keywords'.
	(if (integerp jaspace-highlight-tabs)
		(set (make-local-variable 'jaspace-tab-width) tab-width))
	;; update font lock keywords.
	(set (make-local-variable 'jaspace-font-lock-keywords)
		 (jaspace-font-lock-keywords))
	(unless font-lock-defaults
	  ;; avoid problem on major modes that do not have font lock support,
	  ;; such as fundamental-mode or text-mode.
	  (setq font-lock-defaults '(nil))
	  (when font-lock-mode
		(font-lock-mode -1)
		(font-lock-mode 1)))
	(font-lock-add-keywords nil jaspace-font-lock-keywords t)
	;; enable advices if necessary.
	(when (= 0 (jaspace-active-count))
	  (mapcar (lambda (fun)
				(ad-enable-advice fun 'around 'jaspace-advice)
				(ad-activate fun))
			  jaspace-advice-functions))
	;; synch with `kill-all-local-variables'. `make-local-hook' is
	;; necessary on Emacs 20.
	(make-local-hook 'change-major-mode-hook)
	(add-hook 'change-major-mode-hook 'jaspace-mode-quit nil t)
	;; update the view.
	(jaspace-fontify-buffer)
	(force-mode-line-update)
	(setq jaspace-mode t)
	(run-hooks 'jaspace-mode-hook)))

(defun jaspace-mode-quit ()
  (let ((jaspace-inhibit-update t))
	(remove-hook 'change-major-mode-hook 'jaspace-mode-quit t)
	;; disable advices if there is no other jaspace-mode buffer.
	(when (<= (jaspace-active-count) 1)
	  (mapcar (lambda (fun)
				(ad-disable-advice fun 'around 'jaspace-advice)
				(ad-deactivate fun)
				(ad-activate fun))
			  jaspace-advice-functions))
	;; restore the original display table.
	(when (local-variable-p 'jaspace-original-display-table)
	  (setq buffer-display-table jaspace-original-display-table)
	  (kill-local-variable 'jaspace-original-display-table))
	(when (local-variable-p 'jaspace-tab-width)
	  ;; remove display properties added by jaspace. this could be done
	  ;; by `font-lock-extra-managed-props', but in which case all display
	  ;; properties that are not managed by jaspace would also be removed.
	  (let ((inhibit-modification-hooks t)
			(inhibit-point-motion-hooks t)
			(inhibit-read-only t)
			(buffer-undo-list t)
			(modified (buffer-modified-p))
			(beg (point-min))
			(end (point-max)))
		(save-excursion
		  (while (setq beg (text-property-any beg end 'jaspace-alt t))
			(let ((end (or (text-property-not-all beg end 'jaspace-alt t)
						   (point-max))))
			  (remove-text-properties beg end '(display "" jaspace-alt t))
			  (setq beg end))))
		(or modified
			(if (fboundp 'restore-buffer-modified-p)
				(restore-buffer-modified-p nil)
			  (set-buffer-modified-p nil))))
	  (kill-local-variable 'jaspace-tab-width))
	(jaspace-font-lock-remove-keywords)
	(kill-local-variable 'jaspace-font-lock-keywords)
	;; update the view.
	(jaspace-fontify-buffer)
	(force-mode-line-update)
	(setq jaspace-mode nil)
	(run-hooks 'jaspace-mode-off-hook)))

(defun jaspace-update ()
  (unless jaspace-inhibit-update
	(or (local-variable-p 'jaspace-base-mode)
		(set (make-local-variable 'jaspace-base-mode)
			 (not (null (memq major-mode jaspace-modes)))))
	(cond ((and buffer-read-only
				jaspace-disable-on-read-only-buffers)
		   (if jaspace-mode
			   (jaspace-mode-quit)))
		  (jaspace-follow-font-lock
		   (when jaspace-base-mode
			 (if font-lock-mode
				 (cond ((null jaspace-mode)
						(jaspace-mode-enter))
					   ((and (local-variable-p 'jaspace-tab-width)
							 (not (eq jaspace-tab-width tab-width)))
						(setq jaspace-tab-width tab-width)
						(jaspace-fontify-buffer)))
			   (if jaspace-mode
				   (jaspace-mode-quit)))))
		  ((not (eq jaspace-base-mode jaspace-mode))
		   (if jaspace-base-mode
			   (jaspace-mode-enter)
			 (jaspace-mode-quit)))
		  ((and jaspace-mode
				(local-variable-p 'jaspace-tab-width)
				(not (eq jaspace-tab-width tab-width)))
		   (setq jaspace-tab-width tab-width)
		   (jaspace-fontify-buffer)))
	;; update the timer setting in case it was externally modified.
	(jaspace-set-idle-timer)))

;; * commands

(defun jaspace-mode (&optional arg)
  "Toggle JaSpace minor mode.
With prefix ARG, turn JaSpace mode on if ARG is positive, othrewise off."
  (interactive "P")
  (let ((fun (if (or (and (null arg) (null jaspace-mode))
					 (and arg (< 0 (prefix-numeric-value arg))))
				 'jaspace-mode-on
			   'jaspace-mode-off)))
	(if (interactive-p)
		(call-interactively fun)
	  (funcall fun))))

(defun jaspace-mode-on ()
  "Force JaSpace mode on."
  (interactive)
  (if jaspace-mode
	  (if (interactive-p)
		  (message "jaspace-mode is already active"))
	(let ((jaspace-inhibit-update t))
	  (set (make-local-variable 'jaspace-base-mode) t)
	  (jaspace-mode-enter)
	  (jaspace-set-idle-timer)
	  (if (interactive-p) (message "jaspace-mode is on")
		t))))

(defun jaspace-mode-off ()
  "Force JaSpace mode off."
  (interactive)
  (if (null jaspace-mode)
	  (if (interactive-p)
		  (message "jaspace-mode is not active"))
	(let ((jaspace-inhibit-update t))
	  (set (make-local-variable 'jaspace-base-mode) nil)
	  (jaspace-mode-quit)
	  (jaspace-set-idle-timer)
	  (if (interactive-p) (message "jaspace-mode is off")
		t))))

;; define advices.
(mapcar
 (lambda (fun)
   (eval (list 'defadvice fun '(around jaspace-advice disable)
			   "Temporarily restore buffer's original display table."
			   '(if (and jaspace-mode
						 (local-variable-p 'jaspace-original-display-table))
					(let ((buffer-display-table
						   jaspace-original-display-table))
					  ad-do-it)
				  ad-do-it))))
 jaspace-advice-functions)

;; mmm-mode support... could depend on mmm-mode version?
(defun jaspace-synch-mmm-maybe ()
  (when mmm-mode
	(let ((font-lock-keywords-alist font-lock-keywords-alist)
		  (regions (mmm-regions-alist (point-min) (point-max))))
	  (while regions
		(let ((mode (or (car (car regions))
						major-mode)))
		  (if jaspace-mode
			  (font-lock-add-keywords mode jaspace-font-lock-keywords t))
		  (mmm-update-mode-info mode t))
		(setq regions (cdr regions))))
	(mmm-refontify-maybe)))

(defun jaspace-mmm-mode-hook ()
  (jaspace-synch-mmm-maybe)
  (add-hook 'jaspace-mode-hook 'jaspace-synch-mmm-maybe nil t)
  (add-hook 'jaspace-mode-off-hook 'jaspace-synch-mmm-maybe nil t))

;; add me to the list of minor modes.
(if (boundp 'minor-mode-alist)
	(setq minor-mode-alist
		  (cons '(jaspace-mode jaspace-mode-string)
				(delq (assq 'jaspace-mode minor-mode-alist)
					  minor-mode-alist))))
(if (boundp 'mode-line-mode-menu)
	(define-key mode-line-mode-menu [jaspace-mode]
	  '(menu-item "Japanese Whitespaces" jaspace-mode
				  :button (:toggle . jaspace-mode))))

(jaspace-set-idle-timer)
(provide 'jaspace)

;;; jaspace.el ends here
