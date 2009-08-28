;;; twitter3-mode.el --- Major mode for Twitter

;; Copyright (C) 2007 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;; Created: Sep 4, 2007
;; Version: 0.4
;; Keywords: twitter web
;; URL: http://lambdarepos.svnrepository.com/share/trac.cgi/browser/lang/elisp/twitter3-mode

;; Modified by id774 <idnanashi@gmail.com> to following changes:
;; Divide namespace of twittering-mode into 4 accounts.
;; Change status format.
;; Use global proxy settings.
;; Remove http get success message.
;; Add keybind for window move and quick operation.
;; URL: http://github.com/id774/scripts

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; twitter3-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;; URL : http://twitter.com/d00dle/statuses/577876082
;; URL : http://twitter.com/d00dle/statuses/577879732
;; * Status Input from Popup buffer and C-cC-c to POST.
;; * Mark fav(star)
;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)

(defconst twitter3-mode-version "0.8")

(defun twitter3-mode-version ()
  "Display a message for twitter3-mode version."
  (interactive)
  (let ((version-string
	 (format "twitter3-mode-v%s" twitter3-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar twitter3-mode-map (make-sparse-keymap))

(defvar twitter3-timer nil "Timer object for timeline refreshing will be
stored here. DO NOT SET VALUE MANUALLY.")

(defvar twitter3-idle-time 22)

(defvar twitter3-timer-interval 98)

(defvar twitter3-username nil)

(defvar twitter3-password nil)

(defvar twitter3-last-timeline-retrieved nil)

(defvar twitter3-last-timeline-interactive nil)

(defvar twitter3-new-tweets-count 0
  "Number of new tweets when `twitter3-new-tweets-hook' is run")

(defvar twitter3-new-tweets-hook nil
  "Hook run when new twits are received.

You can read `twitter3-new-tweets-count' to get the number of new
tweets received when this hook is run.")

(defvar twitter3-scroll-mode nil)
(make-variable-buffer-local 'twitter3-scroll-mode)

(defvar twitter3-jojo-mode nil)
(make-variable-buffer-local 'twitter3-jojo-mode)

(defvar twitter3-status-format nil)
(setq twitter3-status-format "%i %s: %t %p [%C]")
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - " in reply to user"
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %f - source
;; %# - id

(defvar twitter3-buffer "*twitter3*")
(defun twitter3-buffer ()
  (twitter3-get-or-generate-buffer twitter3-buffer))

(defvar twitter3-http-buffer "*twitter3-http-buffer*")
(defun twitter3-http-buffer ()
  (twitter3-get-or-generate-buffer twitter3-http-buffer))

(defvar twitter3-timeline-data nil)
(defvar twitter3-timeline-last-update nil)

(defvar twitter3-username-face 'twitter3-username-face)
(defvar twitter3-uri-face 'twitter3-uri-face)

(defun twitter3-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun assocref (item alist)
  (cdr (assoc item alist)))
(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

;;; Proxy
(defvar twitter3-proxy-use global-proxy-use)
(defvar twitter3-proxy-server global-proxy-server)
(defvar twitter3-proxy-port global-proxy-port)
(defvar twitter3-proxy-user global-proxy-user)
(defvar twitter3-proxy-password global-proxy-password)

(defun twitter3-toggle-proxy () ""
  (interactive)
  (setq twitter3-proxy-use
	(not twitter3-proxy-use))
  (message "%s %s"
	   "Use Proxy:"
	   (if twitter3-proxy-use
	       "on" "off")))

(defun twitter3-user-agent-default-function ()
  "twitter3 mode default User-Agent function."
  (concat "Emacs/"
	  (int-to-string emacs-major-version) "." (int-to-string
						   emacs-minor-version)
	  " "
	  "twitter3-mode/"
	  twitter3-mode-version))

(defvar twitter3-sign-simple-string nil)

(defun twitter3-sign-string-default-function ()
  "Tweet append sign string:simple "
  (if twitter3-sign-simple-string
      (concat " [" twitter3-sign-simple-string "]")
    ""))

(defvar twitter3-user-agent-function 'twitter3-user-agent-default-function)
(defvar twitter3-sign-string-function 'twitter3-sign-string-default-function)

(defun twitter3-user-agent ()
  "Return User-Agent header string."
  (funcall twitter3-user-agent-function))

(defun twitter3-sign-string ()
  "Return Tweet sign string."
  (funcall twitter3-sign-string-function))

;;; to show image files

(defvar twitter3-wget-buffer "*twitter3-wget-buffer*")
(defun twitter3-wget-buffer ()
  (twitter3-get-or-generate-buffer twitter3-wget-buffer))

(defvar twitter3-tmp-dir
  (expand-file-name (concat "twmode-images-" (user-login-name))
		    temporary-file-directory))

(defvar twitter3-icon-mode nil "You MUST NOT CHANGE this variable
directory. You should change through function'twitter3-icon-mode'")

(make-variable-buffer-local 'twitter3-icon-mode)
(defun twitter3-icon-mode (&optional arg)
  (interactive)
  (setq twitter3-icon-mode
	(if twitter3-icon-mode
	    (if (null arg)
		nil
	      (> (prefix-numeric-value arg) 0))
	  (when (or (null arg)
		    (and arg (> (prefix-numeric-value arg) 0)))
	    (when (file-writable-p twitter3-tmp-dir)
	      (progn
		(if (not (file-directory-p twitter3-tmp-dir))
		    (make-directory twitter3-tmp-dir))
		t)))))
  (twitter3-render-timeline))

(defun twitter3-scroll-mode (&optional arg)
  (interactive)
  (setq twitter3-scroll-mode
	(if (null arg)
	    (not twitter3-scroll-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun twitter3-jojo-mode (&optional arg)
  (interactive)
  (setq twitter3-jojo-mode
	(if (null arg)
	    (not twitter3-jojo-mode)
	  (> (prefix-numeric-value arg) 0))))

(defvar twitter3-image-stack nil)

(defun twitter3-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun twitter3-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))
(defun twitter3-local-strftime (fmt string)
  (twitter3-setftime fmt string nil))
(defun twitter3-global-strftime (fmt string)
  (twitter3-setftime fmt string t))


(defvar twitter3-debug-mode nil)
(defvar twitter3-debug-buffer "*debug*")
(defun twitter3-debug-buffer ()
  (twitter3-get-or-generate-buffer twitter3-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twitter3-debug-mode
	   (with-current-buffer (twitter3-debug-buffer)
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun twitter3-debug-mode ()
  (interactive)
  (setq twitter3-debug-mode
	(not twitter3-debug-mode))
  (message (if twitter3-debug-mode "debug mode:on" "debug mode:off")))

(if twitter3-mode-map
    (let ((km twitter3-mode-map))
      (define-key km "\C-c\C-f" 'twitter3-friends-timeline)
      (define-key km "e" 'twitter3-friends-timeline)
      (define-key km "\C-c\C-r" 'twitter3-replies-timeline)
      (define-key km "r" 'twitter3-replies-timeline)
      (define-key km "\C-c\C-g" 'twitter3-public-timeline)
      (define-key km "\C-c\C-u" 'twitter3-user-timeline)
      (define-key km "u" 'twitter3-user-timeline)
      (define-key km "\C-c\C-s" 'twitter3-update-status-interactive)
      (define-key km "w" 'twitter3-update-status-interactive)
      (define-key km "\C-c\C-d" 'twitter3-erase-old-statuses)
      (define-key km "\C-m" 'twitter3-enter)
      (define-key km [mouse-1] 'twitter3-click)
      (define-key km "\C-c\C-v" 'twitter3-view-user-page)
      (define-key km "\C-c\C-m" 'twitter3-retweet)
      (define-key km "m" 'twitter3-retweet)
      (define-key km "\C-c\C-n" 'twitter3-naruhodius)
      (define-key km "g" 'twitter3-current-timeline)
      (define-key km "c" 'twitter3-current-timeline-interactive)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twitter3-goto-next-status)
      (define-key km "k" 'twitter3-goto-previous-status)
      ;; (define-key km "l" 'forward-char)
      ;; (define-key km "h" 'backward-char)
      (define-key km "l" 'windmove-right)
      (define-key km "h" 'windmove-left)
      (define-key km "b" 'scroll-down)
      (define-key km "f" 'scroll-up)
      (define-key km " " 'scroll-up)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      ;; (define-key km "n" 'twitter3-goto-next-status-of-user)
      ;; (define-key km "p" 'twitter3-goto-previous-status-of-user)
      (define-key km "n" 'windmove-down)
      (define-key km "p" 'windmove-up)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "\C-c\ i" 'twitter3-icon-mode)
      (define-key km "s" 'twitter3-scroll-mode)
      (define-key km "t" 'twitter3-toggle-proxy)
      (define-key km "\C-c\C-p" 'twitter3-toggle-proxy)
      nil))

(defvar twitter3-mode-syntax-table nil "")

(if twitter3-mode-syntax-table
    ()
  (setq twitter3-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twitter3-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twitter3-mode-syntax-table)
  )

(defun twitter3-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface twitter3-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twitter3-username-face)
  (set-face-attribute 'twitter3-username-face nil :underline t)
  (defface twitter3-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twitter3-uri-face nil :underline t)
  (add-to-list 'minor-mode-alist '(twitter3-icon-mode " tw-icon"))
  (add-to-list 'minor-mode-alist '(twitter3-scroll-mode " tw-scroll"))
  (add-to-list 'minor-mode-alist '(twitter3-jojo-mode " tw-jojo"))
  )

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro twitter3-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar twitter3-mode-string "twitter3 mode")

(defvar twitter3-mode-hook nil
  "twitter3-mode hook.")

(defun twitter3-mode ()
  "Major mode for Twitter
\\{twitter3-mode-map}"
  (interactive)
  (switch-to-buffer (twitter3-buffer))
  (kill-all-local-variables)
  (twitter3-mode-init-variables)
  (use-local-map twitter3-mode-map)
  (setq major-mode 'twitter3-mode)
  (setq mode-name twitter3-mode-string)
  (set-syntax-table twitter3-mode-syntax-table)
  (run-hooks 'twitter3-mode-hook)
  (font-lock-mode -1)
  (twitter3-start))

;;;
;;; Basic HTTP functions
;;;

(defun twitter3-http-get (method-class method &optional parameters sentinel)
  (if (null sentinel) (setq sentinel 'twitter3-http-get-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter3-http-buffer))
    (erase-buffer))

  (let (proc server port
	     (proxy-user twitter3-proxy-user)
	     (proxy-password twitter3-proxy-password))
    (condition-case nil
	(progn
	  (if (and twitter3-proxy-use twitter3-proxy-server)
	      (setq server twitter3-proxy-server
		    port (if (integerp twitter3-proxy-port)
			     (int-to-string twitter3-proxy-port)
			   twitter3-proxy-port))
	    (setq server "twitter.com"
		  port "80"))
	  (setq proc
		(open-network-stream
		 "network-connection-process" (twitter3-http-buffer)
		 server (string-to-number port)))
	  (set-process-sentinel proc sentinel)
	  (process-send-string
	   proc
	   (let ((nl "\r\n")
		 request)
	     (setq request
		   (concat "GET http://twitter.com/" method-class "/" method
			   ".xml"
			   (when parameters
			     (concat "?"
				     (mapconcat
				      (lambda (param-pair)
					(format "%s=%s"
						(twitter3-percent-encode (car
									    param-pair))
						(twitter3-percent-encode (cdr
									    param-pair))))
				      parameters
				      "&")))
			   " HTTP/1.1" nl
			   "Host: twitter.com" nl
			   "User-Agent: " (twitter3-user-agent) nl
			   "Authorization: Basic "
			   (base64-encode-string
			    (concat twitter3-username ":"
				    (twitter3-get-password)))
			   nl
			   "Accept: text/xml"
			   ",application/xml"
			   ",application/xhtml+xml"
			   ",application/html;q=0.9"
			   ",text/plain;q=0.8"
			   ",image/png,*/*;q=0.5" nl
			   "Accept-Charset: utf-8;q=0.7,*;q=0.7" nl
			   (when twitter3-proxy-use
			     "Proxy-Connection: Keep-Alive" nl
			     (when (and proxy-user proxy-password)
			       (concat
				"Proxy-Authorization: Basic "
				(base64-encode-string
				 (concat proxy-user ":"
					 proxy-password))
				nl)))
			   nl))
	     (debug-print (concat "GET Request\n" request))
	     request)))
      (error
       (message "Failure: HTTP GET") nil))))

(defun twitter3-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (twitter3-get-response-header))
	(body (twitter3-get-response-body))
	(status nil)
	)
    (if (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header)
	(progn
	  (setq status (match-string-no-properties 1 header))
	  (case-string
	   status
	   (("200 OK")
	    (setq twitter3-new-tweets-count
		  (count t (mapcar
			    #'twitter3-cache-status-datum
			    (reverse (twitter3-xmltree-to-status
				      body)))))
	    (if (and (> twitter3-new-tweets-count 0)
		     (not twitter3-last-timeline-interactive))
		(run-hooks 'twitter3-new-tweets-hook))
	    (setq twitter3-last-timeline-interactive t)
	    (twitter3-render-timeline))
	   (t (message status))))
      (message "Failure: Bad http response.")))
  )

(defun twitter3-render-timeline ()
  (with-current-buffer (twitter3-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (twitter3-format-status
		       status twitter3-status-format))
	      (fill-region-as-paragraph
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    twitter3-timeline-data)
      (if (and twitter3-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twitter3-scroll-mode (- (point-max) end) 0))))
    ))

(defun twitter3-format-status (status format-str)
  (flet ((attr (key)
	       (assocref key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url))
		(icon-string "\n  "))
	    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
		(let ((filename (match-string-no-properties 1
							    profile-image-url)))
		  ;; download icons if does not exist
		  (if (file-exists-p (concat twitter3-tmp-dir
					     "/" filename))
		      t
		    (add-to-list 'twitter3-image-stack profile-image-url))

		  (when (and icon-string twitter3-icon-mode)
		    (set-text-properties
		     1 2 `(display
			   (image :type ,(twitter3-image-type filename)
				  :file ,(concat twitter3-tmp-dir
						 "/"
						 filename)))
		     icon-string)
		    icon-string)
		  )))))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)"
					  format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (list-push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))

	(case c
	  ((?s)                         ; %s - screen_name
	   (list-push (attr 'user-screen-name) result))
	  ((?S)                         ; %S - name
	   (list-push (attr 'user-name) result))
	  ((?i)                         ; %i - profile_image
	   (list-push (profile-image) result))
	  ((?d)                         ; %d - description
	   (list-push (attr 'user-description) result))
	  ((?l)                         ; %l - location
	   (list-push (attr 'user-location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'user-location)))
	     (unless (or (null location) (string= "" location))
	       (list-push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (list-push (attr 'user-url) result))
	  ((?j)                         ; %j - user.id
	   (list-push (attr 'user-id) result))
	  ((?r)				; %r - in_reply_to_status_id
	   (let ((reply-id (attr 'in-reply-to-status-id))
		 (reply-name (attr 'in-reply-to-screen-name)))
	     (unless (or (null reply-id) (string= "" reply-id)
			 (null reply-name) (string= "" reply-name))
	       (let ((in-reply-to-string (format "in reply to %s" reply-name))
		     (url (twitter3-get-status-url reply-name reply-id)))
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight
			       face twitter3-uri-face
			       uri ,url)
		  in-reply-to-string)
		 (list-push (concat " " in-reply-to-string) result)))))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'user-protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?c)                     ; %c - created_at (raw UTC string)
	   (list-push (attr 'created-at) result))
	  ((?C) ; %C{time-format-str} - created_at (formatted with
	   ; time-format-str)
	   (list-push (twitter3-local-strftime
		       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
		       (attr 'created-at))
		      result))
	  ((?@)                         ; %@ - X seconds ago
	   (let ((created-at
		  (apply
		   'encode-time
		   (parse-time-string (attr 'created-at))))
		 (now (current-time)))
	     (let ((secs (+ (* (- (car now) (car created-at)) 65536)
			    (- (cadr now) (cadr created-at))))
		   time-string url)
	       (setq time-string
		     (cond ((< secs 5) "less than 5 seconds ago")
			   ((< secs 10) "less than 10 seconds ago")
			   ((< secs 20) "less than 20 seconds ago")
			   ((< secs 30) "half a minute ago")
			   ((< secs 60) "less than a minute ago")
			   ((< secs 150) "1 minute ago")
			   ((< secs 2400) (format "%d minutes ago"
						  (/ (+ secs 30) 60)))
			   ((< secs 5400) "about 1 hour ago")
			   ((< secs 84600) (format "about %d hours ago"
						   (/ (+ secs 1800) 3600)))
			   (t (format-time-string "%I:%M %p %B %d, %Y"
						  created-at))))
	       (setq url (twitter3-get-status-url (attr 'user-screen-name)
						    (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face twitter3-uri-face
			     uri ,url)
		time-string)
	       (list-push time-string result))))
	  ((?t)                         ; %t - text
	   (list-push                   ;(clickable-text)
	    (attr 'text)
	    result))
	  ((?')                         ; %' - truncated
	   (let ((truncated (attr 'truncated)))
	     (when (string= "true" truncated)
	       (list-push "..." result))))
	  ((?f)                         ; %f - source
	   (list-push (attr 'source) result))
	  ((?#)                         ; %# - id
	   (list-push (attr 'id) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (list-push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username ,(attr 'user-screen-name)
					id ,(attr 'id)
					text ,(attr 'text))
			     formatted-status)
	formatted-status)
      )))

(defun twitter3-http-post
  (method-class method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com

METHOD-CLASS must be one of Twitter API method classes
 (statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twitter3-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter3-http-buffer))
    (erase-buffer))

  (let (proc server port
	     (proxy-user twitter3-proxy-user)
	     (proxy-password twitter3-proxy-password))
    (progn
      (if (and twitter3-proxy-use twitter3-proxy-server)
	  (setq server twitter3-proxy-server
		port (if (integerp twitter3-proxy-port)
			 (int-to-string twitter3-proxy-port)
		       twitter3-proxy-port))
	(setq server "twitter.com"
	      port "80"))
      (setq proc
	    (open-network-stream
	     "network-connection-process" (twitter3-http-buffer)
	     server (string-to-number port)))
      (set-process-sentinel proc sentinel)
      (process-send-string
       proc
       (let ((nl "\r\n")
	     request)
	 (setq  request
		(concat "POST http://twitter.com/" method-class "/" method ".xml"
			(when parameters
			  (concat "?"
				  (mapconcat
				   (lambda (param-pair)
				     (format "%s=%s"
					     (twitter3-percent-encode (car param-pair))
					     (twitter3-percent-encode (cdr param-pair))))
				   parameters
				   "&")))
			" HTTP/1.1" nl
			"Host: twitter.com" nl
			"User-Agent: " (twitter3-user-agent) nl
			"Authorization: Basic "
			(base64-encode-string
			 (concat twitter3-username ":" (twitter3-get-password)))
			nl
			"Content-Type: text/plain" nl
			"Content-Length: 0" nl
			(when twitter3-proxy-use
			  "Proxy-Connection: Keep-Alive" nl
			  (when (and proxy-user proxy-password)
			    (concat
			     "Proxy-Authorization: Basic "
			     (base64-encode-string
			      (concat proxy-user ":"
				      proxy-password))
			     nl)))
			nl))
	 (debug-print (concat "POST Request\n" request))
	 request)))))

(defun twitter3-http-post-default-sentinel (proc stat &optional suc-msg)

  (condition-case err-signal
      (let ((header (twitter3-get-response-header))
	    ;; (body (twitter3-get-response-body)) not used now.
	    (status nil))
	(string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" header)
	(setq status (match-string-no-properties 1 header))
	(case-string status
		     (("200 OK")
		      (message (if suc-msg suc-msg "Success: Post")))
		     (t (message status)))
	)
    (error (message (prin1-to-string err-signal))))
  )

(defun twitter3-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twitter3-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter3-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring content 0 (string-match "\r?\n\r?\n" content)))))

(defun twitter3-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a
XML tree as list. `buffer' may be a buffer or the name of an existing buffer. If
`buffer' is omitted, the value of `twitter3-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter3-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (let ((content (buffer-string)))
	(xml-parse-region (+ (string-match "\r?\n\r?\n" content)
			     (length (match-string 0 content)))
			  (point-max)))
      )))

(defun twitter3-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twitter3-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twitter3-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (string= id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (if twitter3-jojo-mode
	      (twitter3-update-jojo (cdr (assq 'user-screen-name
						 status-datum))
				      (cdr (assq 'text status-datum))))
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twitter3-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   in-reply-to-status-id
	   in-reply-to-screen-name
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-url
	   user-protected
	   regex-index)

      (setq id (assq-get 'id status-data))
      (setq text (twitter3-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twitter3-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq in-reply-to-status-id
	    (twitter3-decode-html-entities
	     (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
	    (twitter3-decode-html-entities
	     (assq-get 'in_reply_to_screen_name status-data)))
      (setq user-id (assq-get 'id user-data))
      (setq user-name (twitter3-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (twitter3-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (twitter3-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (twitter3-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twitter3-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twitter3-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
	(setq regex-index
	      (string-match "@\\([_a-zA-Z0-9]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
			    text
			    regex-index))
	(when regex-index
	  (let* ((matched-string (match-string-no-properties 0 text))
		 (screen-name (match-string-no-properties 1 text))
		 (uri (match-string-no-properties 2 text)))
	    (add-text-properties
	     (if screen-name
		 (+ 1 (match-beginning 0))
	       (match-beginning 0))
	     (match-end 0)
	     (if screen-name
		 `(mouse-face
		   highlight
		   face twitter3-uri-face
		   uri ,(concat "http://twitter.com/" screen-name))
	       `(mouse-face highlight
			    face twitter3-uri-face
			    uri ,uri))
	     text))
	  (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
	  (let ((uri (match-string-no-properties 1 source))
		(caption (match-string-no-properties 2 source)))
	    (setq source caption)
	    (add-text-properties
	     0 (length source)
	     `(mouse-face highlight
			  uri ,uri
			  face twitter3-uri-face
			  source ,source)
	     source)
	    ))

      ;; save last update time
      (setq twitter3-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    in-reply-to-status-id
	    in-reply-to-screen-name
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-url
	    user-protected)))))

(defun twitter3-xmltree-to-status (xmltree)
  (mapcar #'twitter3-status-to-status-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
	  (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
	    (while statuses
	      (if (consp (car statuses))
		  (setq ret (cons (car statuses) ret)))
	      (setq statuses (cdr statuses)))
	    ret)))

(defun twitter3-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	  (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twitter3-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter3-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twitter3-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (list-push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (list-push
		    (char-to-string
		     (twitter3-ucs-to-char
		      (string-to-number number-entity))) result))
		  (letter-entity
		   (cond ((string= "gt" letter-entity) (list-push ">" result))
			 ((string= "lt" letter-entity) (list-push "<" result))
			 (t (list-push "?" result))))
		  (t (list-push "?" result)))
	    (setq cursor (match-end 0))))
	(list-push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

(defun twitter3-timer-action (func)
  (let ((buf (get-buffer twitter3-buffer)))
    (if (null buf)
	(twitter3-stop)
      (funcall func)
      )))

(defun twitter3-update-status-if-not-blank (status &optional reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (setq status (concat status (twitter3-sign-string)))
    (let ((parameters `(("status" . ,status)
			("source" . "twmode")
			,@(if reply-to-id
			      `(("in_reply_to_status_id"
				 . ,reply-to-id))))))
      (twitter3-http-post "statuses" "update" parameters))
    t))

(defun twitter3-update-status-from-minibuffer (&optional init-str
							   reply-to-id)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (while not-posted-p
      (setq status (read-from-minibuffer "status: " status nil nil nil nil t))
      (setq not-posted-p
	    (not (twitter3-update-status-if-not-blank status reply-to-id))))
    ))

(defun twitter3-update-lambda ()
  (interactive)
  (twitter3-http-post
   "statuses" "update"
   `(("status" . "\xd34b\xd22b\xd26f\xd224\xd224\xd268\xd34b")
     ("source" . "twmode"))))

(defun twitter3-update-jojo (usr msg)
  (if (string-match "\xde21\xd24b\\(\xd22a\xe0b0\\|\xdaae\xe6cd\\)\xd24f\xd0d6\\([^\xd0d7]+\\)\xd0d7\xd248\xdc40\xd226"
		    msg)
      (twitter3-http-post
       "statuses" "update"
       `(("status" . ,(concat
		       "@" usr " "
		       (match-string-no-properties 2 msg)
		       "\xd0a1\xd24f\xd243!?"))
	 ("source" . "twmode")))))

;;;
;;; Commands
;;;

(defun twitter3-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twitter3-current-timeline-noninteractive))
  (if twitter3-timer
      nil
    (setq twitter3-timer
	  (run-at-time "0 sec"
		       twitter3-timer-interval
		       #'twitter3-timer-action action))))

(defun twitter3-stop ()
  (interactive)
  (cancel-timer twitter3-timer)
  (setq twitter3-timer nil))

(defun twitter3-get-timeline (method)
  (if (not (eq twitter3-last-timeline-retrieved method))
      (setq twitter3-timeline-last-update nil
	    twitter3-timeline-data nil))
  (setq twitter3-last-timeline-retrieved method)
  (let ((buf (get-buffer twitter3-buffer)))
    (if (not buf)
	(twitter3-stop)
      (if (not twitter3-timeline-last-update)
	  (twitter3-http-get "statuses" method)
	(let* ((system-time-locale "C")
	       (since
		(twitter3-global-strftime
		 "%a, %d %b %Y %H:%M:%S JST"
		 twitter3-timeline-last-update)))
	  (twitter3-http-get "statuses" method
			       `(("since" . ,since)))))))

  (if (and twitter3-icon-mode window-system)
      (if twitter3-image-stack
	  (let ((proc
		 (apply
		  #'start-process
		  "wget-images"
		  (twitter3-wget-buffer)
		  "wget"
		  (format "--directory-prefix=%s" twitter3-tmp-dir)
		  "--no-clobber"
		  "--quiet"
		  twitter3-image-stack)))
	    (set-process-sentinel
	     proc
	     (lambda (proc stat)
	       (clear-image-cache)
	       (save-excursion
		 (set-buffer (twitter3-wget-buffer))
		 )))))))

(defun twitter3-friends-timeline ()
  (interactive)
  (twitter3-get-timeline "friends_timeline"))

(defun twitter3-replies-timeline ()
  (interactive)
  (twitter3-get-timeline "replies"))

(defun twitter3-public-timeline ()
  (interactive)
  (twitter3-get-timeline "public_timeline"))

(defun twitter3-user-timeline ()
  (interactive)
  (twitter3-get-timeline "user_timeline"))

(defun twitter3-current-timeline-interactive ()
  (interactive)
  (setq twitter3-last-timeline-interactive t)
  (twitter3-current-timeline))

(defun twitter3-current-timeline-noninteractive ()
  (setq twitter3-last-timeline-interactive nil)
  (twitter3-current-timeline))

(defun twitter3-current-timeline ()
  (if (not twitter3-last-timeline-retrieved)
      (setq twitter3-last-timeline-retrieved "friends_timeline"))
  (twitter3-get-timeline twitter3-last-timeline-retrieved))

(defun twitter3-update-status-interactive ()
  (interactive)
  (twitter3-update-status-from-minibuffer))

(defun twitter3-erase-old-statuses ()
  (interactive)
  (setq twitter3-timeline-data nil)
  (if (not twitter3-last-timeline-retrieved)
      (setq twitter3-last-timeline-retrieved "friends_timeline"))
  (if (not twitter3-timeline-last-update)
      (twitter3-http-get "statuses" twitter3-last-timeline-retrieved)
    (let* ((system-time-locale "C")
	   (since
	    (twitter3-global-strftime
	     "%a, %d %b %Y %H:%M:%S JST"
	     twitter3-timeline-last-update)))
      (twitter3-http-get "statuses" twitter3-last-timeline-retrieved
			   `(("since" . ,since))))))

(defun twitter3-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twitter3-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(uri (get-text-property (point) 'uri)))
    (if username
	(twitter3-update-status-from-minibuffer (concat "@" username " ") id)
      (if uri
	  (browse-url uri)))))

(defun twitter3-retweet ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(text (get-text-property (point) 'text)))
    (when username
	(twitter3-update-status-from-minibuffer
	  (concat "のっかりつぶやき: @" username " " text ) id))))

(defun twitter3-naruhodius ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(text (get-text-property (point) 'text)))
    (when username
	(twitter3-update-status-from-minibuffer
	  (concat "ナルホディウス: " text " (via @" username ")") id))))

(defun twitter3-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twitter3-other-user-timeline ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
	(twitter3-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twitter3-other-user-timeline-interactive ()
  (interactive)
  (let ((username (read-from-minibuffer "user: " (get-text-property (point) 'username))))
    (if (> (length username) 0)
	(twitter3-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twitter3-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twitter3-update-status-from-minibuffer (concat "@" username " ")))))

(defun twitter3-get-password ()
  (or twitter3-password
      (setq twitter3-password (read-passwd "twitter3-mode: "))))

(defun twitter3-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twitter3-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "End of status."))))

(defun twitter3-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter3-username-face)))
	(setq pos (next-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter3-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (twitter3-get-previous-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "Start of status."))))

(defun twitter3-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter3-username-face)))
	(setq pos (previous-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter3-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twitter3-get-username-at-pos (point)))
	(pos (twitter3-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twitter3-get-username-at-pos pos) user-name)))
      (setq pos (twitter3-get-next-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twitter3-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twitter3-get-username-at-pos (point)))
	(pos (twitter3-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twitter3-get-username-at-pos pos) user-name)))
      (setq pos (twitter3-get-previous-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twitter3-get-username-at-pos (pos)
  (let ((start-pos pos)
	(end-pos))
    (catch 'not-found
      (while (eq (get-text-property start-pos 'face) twitter3-username-face)
	(setq start-pos (1- start-pos))
	(when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun twitter3-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%s" username id))

;;;###autoload
(defun twit ()
  "Start twitter3-mode."
  (interactive)
  (twitter3-mode))

(provide 'twitter3-mode)
;;; twitter3.el ends here
