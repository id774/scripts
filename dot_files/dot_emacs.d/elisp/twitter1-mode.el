;;; twitter1-mode.el --- Major mode for Twitter

;; Copyright (C) 2007 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;; Created: Sep 4, 2007
;; Version: 0.4
;; Keywords: twitter web
;; URL: http://lambdarepos.svnrepository.com/share/trac.cgi/browser/lang/elisp/twitter1-mode

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

;; twitter1-mode.el is a major mode for Twitter.
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

(defconst twitter1-mode-version "0.6")

(defun twitter1-mode-version ()
  "Display a message for twitter1-mode version."
  (interactive)
  (let ((version-string
	 (format "twitter1-mode-v%s" twitter1-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar twitter1-mode-map (make-sparse-keymap))

(defvar twitter1-timer nil "Timer object for timeline refreshing will be stored here. DO NOT SET VALUE MANUALLY.")

(defvar twitter1-idle-time 30)

(defvar twitter1-timer-interval 300)

(defvar twitter1-username nil)

(defvar twitter1-password nil)

(defvar twitter1-scroll-mode nil)
(make-variable-buffer-local 'twitter1-scroll-mode)

(defvar twitter1-jojo-mode nil)
(make-variable-buffer-local 'twitter1-jojo-mode)

(defvar twitter1-status-format nil)
(setq twitter1-status-format "%i %s,  %@:\n  %t // from %f%L")
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
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

(defvar twitter1-buffer "*twitter1*")
(defun twitter1-buffer ()
  (twitter1-get-or-generate-buffer twitter1-buffer))

(defvar twitter1-http-buffer "*twitter1-http-buffer*")
(defun twitter1-http-buffer ()
  (twitter1-get-or-generate-buffer twitter1-http-buffer))

(defvar twitter1-friends-timeline-data nil)
(defvar twitter1-friends-timeline-last-update nil)

(defvar twitter1-username-face 'twitter1-username-face)
(defvar twitter1-uri-face 'twitter1-uri-face)

(defun twitter1-get-or-generate-buffer (buffer)
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
(defvar twitter1-proxy-use nil)
(defvar twitter1-proxy-server nil)
(defvar twitter1-proxy-port 8080)
(defvar twitter1-proxy-user nil)
(defvar twitter1-proxy-password nil)

(defun twitter1-toggle-proxy () ""
  (interactive)
  (setq twitter1-proxy-use
	(not twitter1-proxy-use))
  (message "%s %s"
	   "Use Proxy:"
	   (if twitter1-proxy-use
	       "on" "off")))

(defun twitter1-user-agent-default-function ()
  "twitter1 mode default User-Agent function."
  (concat "Emacs/"
	  (int-to-string emacs-major-version) "." (int-to-string
						   emacs-minor-version)
	  " "
	  "twitter1-mode/"
	  twitter1-mode-version))

(defvar twitter1-user-agent-function 'twitter1-user-agent-default-function)

(defun twitter1-user-agent ()
  "Return User-Agent header string."
  (funcall twitter1-user-agent-function))

;;; to show image files

(defvar twitter1-wget-buffer "*twitter1-wget-buffer*")
(defun twitter1-wget-buffer ()
  (twitter1-get-or-generate-buffer twitter1-wget-buffer))

(defvar twitter1-tmp-dir
  (expand-file-name (concat "twmode-images-" (user-login-name))
		    temporary-file-directory))

(defvar twitter1-icon-mode nil "You MUST NOT CHANGE this variable directory. You should change through function'twitter1-icon-mode'")
(make-variable-buffer-local 'twitter1-icon-mode)
(defun twitter1-icon-mode (&optional arg)
  (interactive)
  (setq twitter1-icon-mode
	(if twitter1-icon-mode
	    (if (null arg)
		nil
	      (> (prefix-numeric-value arg) 0))
	  (when (or (null arg)
		    (and arg (> (prefix-numeric-value arg) 0)))
	    (when (file-writable-p twitter1-tmp-dir)
	      (progn
		(if (not (file-directory-p twitter1-tmp-dir))
		    (make-directory twitter1-tmp-dir))
		t)))))
  (twitter1-render-friends-timeline))

(defun twitter1-scroll-mode (&optional arg)
  (interactive)
  (setq twitter1-scroll-mode
	(if (null arg)
	    (not twitter1-scroll-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun twitter1-jojo-mode (&optional arg)
  (interactive)
  (setq twitter1-jojo-mode
	(if (null arg)
	    (not twitter1-jojo-mode)
	  (> (prefix-numeric-value arg) 0))))

(defvar twitter1-image-stack nil)

(defun twitter1-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun twitter1-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))
(defun twitter1-local-strftime (fmt string)
  (twitter1-setftime fmt string nil))
(defun twitter1-global-strftime (fmt string)
  (twitter1-setftime fmt string t))


(defvar twitter1-debug-mode nil)
(defvar twitter1-debug-buffer "*debug*")
(defun twitter1-debug-buffer ()
  (twitter1-get-or-generate-buffer twitter1-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twitter1-debug-mode
	   (with-current-buffer (twitter1-debug-buffer)
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun twitter1-debug-mode ()
  (interactive)
  (setq twitter1-debug-mode
	(not twitter1-debug-mode))
  (message (if twitter1-debug-mode "debug mode:on" "debug mode:off")))

(if twitter1-mode-map
    (let ((km twitter1-mode-map))
      (define-key km "\C-c\C-f" 'twitter1-friends-timeline)
      (define-key km "\C-c\C-s" 'twitter1-update-status-interactive)
      (define-key km "\C-c\C-e" 'twitter1-erase-old-statuses)
      (define-key km "\C-m" 'twitter1-enter)
      (define-key km "\C-c\C-l" 'twitter1-update-lambda)
      (define-key km [mouse-1] 'twitter1-click)
      (define-key km "\C-c\C-v" 'twitter1-view-user-page)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twitter1-goto-next-status)
      (define-key km "k" 'twitter1-goto-previous-status)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'twitter1-goto-next-status-of-user)
      (define-key km "p" 'twitter1-goto-previous-status-of-user)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'twitter1-icon-mode)
      (define-key km "s" 'twitter1-scroll-mode)
      (define-key km "t" 'twitter1-toggle-proxy)
      (define-key km "\C-c\C-p" 'twitter1-toggle-proxy)
      nil))

(defvar twitter1-mode-syntax-table nil "")

(if twitter1-mode-syntax-table
    ()
  (setq twitter1-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twitter1-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twitter1-mode-syntax-table)
  )

(defun twitter1-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface twitter1-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twitter1-username-face)
  (set-face-attribute 'twitter1-username-face nil :underline t)
  (defface twitter1-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twitter1-uri-face nil :underline t)
  (add-to-list 'minor-mode-alist '(twitter1-icon-mode " tw-icon"))
  (add-to-list 'minor-mode-alist '(twitter1-scroll-mode " tw-scroll"))
  (add-to-list 'minor-mode-alist '(twitter1-jojo-mode " tw-jojo"))
  )

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key)) keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro twitter1-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar twitter1-mode-string "twitter1 mode")

(defvar twitter1-mode-hook nil
  "twitter1-mode hook.")

(defun twitter1-mode ()
  "Major mode for Twitter"
  (interactive)
  (switch-to-buffer (twitter1-buffer))
  (kill-all-local-variables)
  (twitter1-mode-init-variables)
  (use-local-map twitter1-mode-map)
  (setq major-mode 'twitter1-mode)
  (setq mode-name twitter1-mode-string)
  (set-syntax-table twitter1-mode-syntax-table)
  (run-hooks 'twitter1-mode-hook)
  (font-lock-mode -1)
  (twitter1-start)
  )

;;;
;;; Basic HTTP functions
;;;

(defun twitter1-http-get (method-class method &optional parameters sentinel)
  (if (null sentinel) (setq sentinel 'twitter1-http-get-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter1-http-buffer))
    (erase-buffer))

  (let (proc server port
	     (proxy-user twitter1-proxy-user)
	     (proxy-password twitter1-proxy-password))
    (condition-case nil
	(progn
	  (if (and twitter1-proxy-use twitter1-proxy-server)
	      (setq server twitter1-proxy-server
		    port (if (integerp twitter1-proxy-port)
			     (int-to-string twitter1-proxy-port)
			   twitter1-proxy-port))
	    (setq server "twitter.com"
		  port "80"))
	  (setq proc
		(open-network-stream
		 "network-connection-process" (twitter1-http-buffer)
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
						(twitter1-percent-encode (car param-pair))
						(twitter1-percent-encode (cdr param-pair))))
				      parameters
				      "&")))
			   " HTTP/1.1" nl
			   "Host: twitter.com" nl
			   "User-Agent: " (twitter1-user-agent) nl
			   "Authorization: Basic "
			   (base64-encode-string
			    (concat twitter1-username ":" (twitter1-get-password)))
			   nl
			   "Accept: text/xml"
			   ",application/xml"
			   ",application/xhtml+xml"
			   ",application/html;q=0.9"
			   ",text/plain;q=0.8"
			   ",image/png,*/*;q=0.5" nl
			   "Accept-Charset: utf-8;q=0.7,*;q=0.7" nl
			   (when twitter1-proxy-use
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

(defun twitter1-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (twitter1-get-response-header))
	(body (twitter1-get-response-body))
	(status nil)
	)
    (if (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header)
	(progn
	  (setq status (match-string-no-properties 1 header))
	  (case-string
	   status
	   (("200 OK")
	    (mapcar
	     #'twitter1-cache-status-datum
	     (reverse (twitter1-xmltree-to-status
		       body)))
	    (twitter1-render-friends-timeline)
	    (message (if suc-msg suc-msg "Success: Get.")))
	   (t (message status))))
      (message "Failure: Bad http response.")))
  )

(defun twitter1-render-friends-timeline ()
  (with-current-buffer (twitter1-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (twitter1-format-status
		       status twitter1-status-format))
	      (fill-region-as-paragraph
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    twitter1-friends-timeline-data)
      (if twitter1-image-stack
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twitter1-scroll-mode (- (point-max) end) 0))))
    ))

(defun twitter1-format-status (status format-str)
  (flet ((attr (key)
	       (assocref key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url))
		(icon-string "\n  "))
	    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
		(let ((filename (match-string-no-properties 1 profile-image-url)))
		  ;; download icons if does not exist
		  (if (file-exists-p (concat twitter1-tmp-dir
					     "/" filename))
		      t
		    (add-to-list 'twitter1-image-stack profile-image-url))

		  (when (and icon-string twitter1-icon-mode)
		    (set-text-properties
		     1 2 `(display
			   (image :type ,(twitter1-image-type filename)
				  :file ,(concat twitter1-tmp-dir
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
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)" format-str cursor))
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
	   (list-push (format "%d" (attr 'user-id)) result))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'user-protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?c)                     ; %c - created_at (raw UTC string)
	   (list-push (attr 'created-at) result))
	  ((?C) ; %C{time-format-str} - created_at (formatted with time-format-str)
	   (list-push (twitter1-local-strftime
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
			   (t (format-time-string "%I:%M %p %B %d, %Y" created-at))))
	       (setq url (twitter1-get-status-url (attr 'user-screen-name) (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face twitter1-uri-face
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
	   (list-push (format "%d" (attr 'id)) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (list-push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username ,(attr 'user-screen-name))
			     formatted-status)
	formatted-status)
      )))

(defun twitter1-http-post
  (method-class method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com

METHOD-CLASS must be one of Twitter API method classes(statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twitter1-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter1-http-buffer))
    (erase-buffer))

  (let (proc server port
	     (proxy-user twitter1-proxy-user)
	     (proxy-password twitter1-proxy-password))
    (progn
      (if (and twitter1-proxy-use twitter1-proxy-server)
	  (setq server twitter1-proxy-server
		port (if (integerp twitter1-proxy-port)
			 (int-to-string twitter1-proxy-port)
		       twitter1-proxy-port))
	(setq server "twitter.com"
	      port "80"))
      (setq proc
	    (open-network-stream
	     "network-connection-process" (twitter1-http-buffer)
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
					     (twitter1-percent-encode (car param-pair))
					     (twitter1-percent-encode (cdr param-pair))))
				   parameters
				   "&")))
			" HTTP/1.1" nl
			"Host: twitter.com" nl
			"User-Agent: " (twitter1-user-agent) nl
			"Authorization: Basic "
			(base64-encode-string
			 (concat twitter1-username ":" (twitter1-get-password)))
			nl
			"Content-Type: text/plain" nl
			"Content-Length: 0" nl
			(when twitter1-proxy-use
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

(defun twitter1-http-post-default-sentinel (proc stat &optional suc-msg)

  (condition-case err-signal
      (let ((header (twitter1-get-response-header))
	    ;; (body (twitter1-get-response-body)) not used now.
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

(defun twitter1-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twitter1-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter1-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring content 0 (string-match "\r?\n\r?\n" content)))))

(defun twitter1-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twitter1-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter1-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (let ((content (buffer-string)))
	(xml-parse-region (+ (string-match "\r?\n\r?\n" content)
			     (length (match-string 0 content)))
			  (point-max)))
      )))

(defun twitter1-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twitter1-friends-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twitter1-friends-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (eql id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (if twitter1-jojo-mode
	      (twitter1-update-jojo (cdr (assq 'user-screen-name status-datum))
				      (cdr (assq 'text status-datum))))
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twitter1-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-url
	   user-protected
	   regex-index)

      (setq id (string-to-number (assq-get 'id status-data)))
      (setq text (twitter1-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twitter1-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq user-id (string-to-number (assq-get 'id user-data)))
      (setq user-name (twitter1-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (twitter1-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (twitter1-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (twitter1-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twitter1-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    face twitter1-username-face
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twitter1-username-face)
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
		   face twitter1-uri-face
		   uri ,(concat "http://twitter.com/" screen-name))
	       `(mouse-face highlight
			    face twitter1-uri-face
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
			  face twitter1-uri-face
			  source ,source)
	     source)
	    ))

      ;; save last update time
      (setq twitter1-friends-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-url
	    user-protected)))))

(defun twitter1-xmltree-to-status (xmltree)
  (mapcar #'twitter1-status-to-status-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
	  (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
	    (while statuses
	      (if (consp (car statuses))
		  (setq ret (cons (car statuses) ret)))
	      (setq statuses (cdr statuses)))
	    ret)))

(defun twitter1-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	  (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twitter1-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter1-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twitter1-decode-html-entities (encoded-str)
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
		     (twitter1-ucs-to-char
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

(defun twitter1-timer-action (func)
  (let ((buf (get-buffer twitter1-buffer)))
    (if (null buf)
	(twitter1-stop)
      (funcall func)
      )))

(defun twitter1-update-status-if-not-blank (status)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (twitter1-http-post "statuses" "update"
			  `(("status" . ,status)
			    ("source" . "twmode")))
    t))

(defun twitter1-update-status-from-minibuffer (&optional init-str)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (while not-posted-p
      (setq status (read-from-minibuffer "status: " status nil nil nil nil t))
      (setq not-posted-p
	    (not (twitter1-update-status-if-not-blank status))))))

(defun twitter1-update-lambda ()
  (interactive)
  (twitter1-http-post
   "statuses" "update"
   `(("status" . "\xd34b\xd22b\xd26f\xd224\xd224\xd268\xd34b")
     ("source" . "twmode"))))

(defun twitter1-update-jojo (usr msg)
  (if (string-match "\xde21\xd24b\\(\xd22a\xe0b0\\|\xdaae\xe6cd\\)\xd24f\xd0d6\\([^\xd0d7]+\\)\xd0d7\xd248\xdc40\xd226"
		    msg)
      (twitter1-http-post
       "statuses" "update"
       `(("status" . ,(concat
		       "@" usr " "
		       (match-string-no-properties 2 msg)
		       "\xd0a1\xd24f\xd243!?"))
	 ("source" . "twmode")))))

;;;
;;; Commands
;;;

(defun twitter1-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twitter1-friends-timeline))
  (if twitter1-timer
      nil
    (setq twitter1-timer
	  (run-at-time "0 sec"
		       twitter1-timer-interval
		       #'twitter1-timer-action action))))

(defun twitter1-stop ()
  (interactive)
  (cancel-timer twitter1-timer)
  (setq twitter1-timer nil))

(defun twitter1-friends-timeline ()
  (interactive)
  (let ((buf (get-buffer twitter1-buffer)))
    (if (not buf)
	(twitter1-stop)
       (if (not twitter1-friends-timeline-last-update)
	   (twitter1-http-get "statuses" "friends_timeline")
	 (let* ((system-time-locale "C")
		(since
		  (twitter1-global-strftime
		   "%a, %d %b %Y %H:%M:%S GMT"
		   twitter1-friends-timeline-last-update)))
	   (twitter1-http-get "statuses" "friends_timeline"
				`(("since" . ,since)))))))

  (if twitter1-icon-mode
      (if twitter1-image-stack
	  (let ((proc
		 (apply
		  #'start-process
		  "wget-images"
		  (twitter1-wget-buffer)
		  "wget"
		  (format "--directory-prefix=%s" twitter1-tmp-dir)
		  "--no-clobber"
		  "--quiet"
		  twitter1-image-stack)))
	    (set-process-sentinel
	     proc
	     (lambda (proc stat)
	       (clear-image-cache)
	       (save-excursion
		 (set-buffer (twitter1-wget-buffer))
		 )))))))

(defun twitter1-update-status-interactive ()
  (interactive)
  (twitter1-update-status-from-minibuffer))

(defun twitter1-erase-old-statuses ()
  (interactive)
  (setq twitter1-friends-timeline-data nil)
  (if (not twitter1-friends-timeline-last-update)
      (twitter1-http-get "statuses" "friends_timeline")
    (let* ((system-time-locale "C")
	   (since
	     (twitter1-global-strftime
	      "%a, %d %b %Y %H:%M:%S GMT"
	      twitter1-friends-timeline-last-update)))
      (twitter1-http-get "statuses" "friends_timeline"
			   `(("since" . ,since))))))

(defun twitter1-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twitter1-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(uri (get-text-property (point) 'uri)))
    (if username
	(twitter1-update-status-from-minibuffer (concat "@" username " "))
      (if uri
	  (browse-url uri)))))

(defun twitter1-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twitter1-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twitter1-update-status-from-minibuffer (concat "@" username " ")))))

(defun twitter1-get-password ()
  (or twitter1-password
      (setq twitter1-password (read-passwd "twitter1-mode: "))))

(defun twitter1-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twitter1-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "End of status."))))

(defun twitter1-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter1-username-face)))
	(setq pos (next-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter1-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (twitter1-get-previous-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "Start of status."))))

(defun twitter1-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter1-username-face)))
	(setq pos (previous-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter1-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twitter1-get-username-at-pos (point)))
	(pos (twitter1-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twitter1-get-username-at-pos pos) user-name)))
      (setq pos (twitter1-get-next-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twitter1-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twitter1-get-username-at-pos (point)))
	(pos (twitter1-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twitter1-get-username-at-pos pos) user-name)))
      (setq pos (twitter1-get-previous-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twitter1-get-username-at-pos (pos)
  (let ((start-pos pos)
	(end-pos))
    (catch 'not-found
      (while (eq (get-text-property start-pos 'face) twitter1-username-face)
	(setq start-pos (1- start-pos))
	(when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun twitter1-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%d" username id))

;;;###autoload
(defun twit ()
  "Start twitter1-mode."
  (interactive)
  (twitter1-mode))

(provide 'twitter1-mode)
;;; twitter1.el ends here
