;;; -*- indent-tabs-mode: t; tab-width: 8 -*-
;;;
;;; twitter1-mode.el --- Major mode for Twitter

;; Copyright (C) 2007, 2009, 2010 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;; Created: Sep 4, 2007
;; Version: 0.9.0
;; Keywords: twitter web
;; URL: http://github.com/hayamiz/twittering-mode/

;; Modified by id774 <idnanashi@gmail.com> to following changes:
;; Divide namespace of twittering-mode into 6 accounts.
;; Change status format.
;; Use my global proxy settings.
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

;; twittering-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;; URL : http://twitter.com/d00dle/statuses/577876082
;; * Status Input from Popup buffer and C-cC-c to POST.
;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'parse-time)
(when (< emacs-major-version 22)
  (add-to-list 'load-path
	       (expand-file-name
		"url-emacs21" (file-name-directory load-file-name)))
  (require 'un-define)
  (set-terminal-coding-system 'utf-8))
(require 'url)

(defconst twitter1-mode-version "0.9.0")

(defun twitter1-mode-version ()
  "Display a message for twitter1-mode version."
  (interactive)
  (let ((version-string
	 (format "twitter1-mode-v%s" twitter1-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defconst twitter1-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twitter1-number-of-tweets-on-retrieval'.")

(defvar twitter1-number-of-tweets-on-retrieval 20
  "*The number of tweets which will be retrieved in one request.
The upper limit is `twitter1-max-number-of-tweets-on-retrieval'.")

(defvar twitter1-tinyurl-service 'tinyurl
  "The service to use. One of 'tinyurl' or 'toly'")

(defvar twitter1-tinyurl-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl="))
  "Alist of tinyfy services")

(defvar twitter1-mode-map (make-sparse-keymap))

(defvar twitter1-tweet-history nil)
(defvar twitter1-user-history nil)
(defvar twitter1-timeline-history nil)
(defvar twitter1-hashtag-history nil)

(defvar twitter1-current-hashtag nil
  "A hash tag string currently set. You can set it by calling
`twitter1-set-current-hashtag'")

(defvar twitter1-timer nil
  "Timer object for timeline refreshing will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twitter1-timer-interval 90
  "The interval of auto reloading. You should use 60 or more
seconds for this variable because the number of API call is
limited by the hour.")

(defvar twitter1-username nil
  "An username of your Twitter account.")
(defvar twitter1-username-active nil
  "Copy of `twitter1-username' for internal use.")

(defvar twitter1-password nil
  "A password of your Twitter account. Leave it blank is the
recommended way because writing a password in .emacs file is so
dangerous.")
(defvar twitter1-password-active nil
  "Copy of `twitter1-password' for internal use.")

(defvar twitter1-initial-timeline-spec-string ":friends"
  "The initial timeline spec string.")

(defvar twitter1-timeline-spec-alias nil
  "*Alist for aliases of timeline spec.
Each element is (NAME . SPEC-STRING), where NAME and SPEC-STRING are
strings. The alias can be referred as \"$NAME\" in timeline spec
string.

For example, if you specify
 '((\"FRIENDS\" . \"(USER1+USER2+USER3)\")
   (\"to_me\" . \"(:mentions+:retweets_of_me+:direct-messages)\")),
then you can use \"$to_me\" as
\"(:mentions+:retweets_of_me+:direct-messages)\".")

(defvar twitter1-last-requested-timeline-spec-string nil
  "The last requested timeline spec string.")
(defvar twitter1-last-retrieved-timeline-spec-string nil
  "The last successfully retrieved timeline spec string.")
(defvar twitter1-list-index-retrieved nil)

(defvar twitter1-new-tweets-count 0
  "Number of new tweets when `twitter1-new-tweets-hook' is run")

(defvar twitter1-new-tweets-hook nil
  "Hook run when new twits are received.

You can read `twitter1-new-tweets-count' to get the number of new
tweets received when this hook is run.")

(defvar twitter1-scroll-mode nil)
(make-variable-buffer-local 'twitter1-scroll-mode)

(defvar twitter1-jojo-mode nil)
(make-variable-buffer-local 'twitter1-jojo-mode)

(defvar twitter1-status-format "%i %s: %t %r%R%p [%C]"
  "Format string for rendering statuses.
Ex. \"%i %s,  %@:\\n%FILL{  %T // from %f%L%r%R}\"

Items:
 %s - screen_name
 %S - name
 %i - profile_image
 %d - description
 %l - location
 %L - \" [location]\"
 %r - \" in reply to user\"
 %R - \" retweeted by user\"
 %u - url
 %j - user.id
 %p - protected?
 %c - created_at (raw UTC string)
 %C{time-format-str} - created_at (formatted with time-format-str)
 %@ - X seconds ago
 %T - raw text
 %t - text filled as one paragraph
 %' - truncated
 %FILL{...} - strings filled as a paragrah.
              You can use any other specifiers in braces.
 %f - source
 %# - id
")

(defvar twitter1-retweet-format "RT: %t (via @%s)"
  "Format string for retweet.

Items:
 %s - screen_name
 %t - text
 %% - %
")

(defvar twitter1-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)")

(defvar twitter1-notify-successful-http-get t)

(defvar twitter1-use-ssl t
  "Use SSL connection if this variable is non-nil.

SSL connections use 'curl' command as a backend.")

(defvar twitter1-buffer "*twitter1*")
(defun twitter1-buffer ()
  (twitter1-get-or-generate-buffer twitter1-buffer))

(defvar twitter1-timeline-data nil)
(defvar twitter1-timeline-last-update nil)

(defvar twitter1-username-face 'twitter1-username-face)
(defvar twitter1-uri-face 'twitter1-uri-face)

(defvar twitter1-use-native-retweet nil
  "Post retweets using native retweets if this variable is non-nil.")

;;;
;;; Proxy setting / functions
;;;

(defvar twitter1-proxy-use global-proxy-use)
(defvar twitter1-proxy-keep-alive nil)
(defvar twitter1-proxy-server global-proxy-server
  "*The proxy server for `twitter1-mode'.
If nil, it is initialized on entering `twitter1-mode'.
The port number is specified by `twitter1-proxy-port'.")
(defvar twitter1-proxy-port global-proxy-port
  "*The port number of a proxy server for `twitter1-mode'.
If nil, it is initialized on entering `twitter1-mode'.
The server is specified by `twitter1-proxy-server'.")
(defvar twitter1-proxy-user global-proxy-user)
(defvar twitter1-proxy-password global-proxy-password)


(defun twitter1-find-proxy (scheme)
  "Find proxy server and its port for `twitter1-mode' and returns
a cons pair of them.
SCHEME must be \"http\" or \"https\"."
  (cond
   ((require 'url-methods nil t)
    (url-scheme-register-proxy scheme)
    (let* ((proxy-service (assoc scheme url-proxy-services))
           (proxy (if proxy-service (cdr proxy-service) nil)))
      (if (and proxy
               (string-match "^\\([^:]+\\):\\([0-9]+\\)$" proxy))
          (let* ((host (match-string 1 proxy))
                 (port (string-to-number (match-string 2 proxy))))
            (cons host port))
        nil)))
   (t
    (let* ((env-var (concat scheme "_proxy"))
           (env-proxy (or (getenv (upcase env-var))
                          (getenv (downcase env-var))))
	   (default-port (if (string= "https" scheme) "443" "80")))
      (if (and env-proxy
	       (string-match
		"^\\(https?://\\)?\\([^:/]+\\)\\(:\\([0-9]+\\)\\)?/?$"
		env-proxy))
          (let* ((host (match-string 2 env-proxy))
		 (port-str (or (match-string 4 env-proxy) default-port))
		 (port (string-to-number port-str)))
            (cons host port))
	nil)))))

(defun twitter1-setup-proxy ()
  (unless (and twitter1-proxy-server twitter1-proxy-port)
    (let ((proxy-info (or (if twitter1-use-ssl
			      (twitter1-find-proxy "https"))
			  (twitter1-find-proxy "http"))))
      (when proxy-info
	(let ((host (car proxy-info))
	      (port (cdr proxy-info)))
	  (setq twitter1-proxy-server host)
	  (setq twitter1-proxy-port port)))))
  (when (and twitter1-proxy-use
	     (null twitter1-proxy-server)
	     (null twitter1-proxy-port))
    (message "Disabling proxy due to lack of configuration.")
    (setq twitter1-proxy-use nil)))

(defun twitter1-toggle-proxy ()
  (interactive)
  (setq twitter1-proxy-use
	(not twitter1-proxy-use))
  (twitter1-update-mode-line)
  (message (if twitter1-proxy-use "Use Proxy:on" "Use Proxy:off")))

;;;
;;; to show image files
;;;

(defvar twitter1-icon-mode nil
  "You MUST NOT CHANGE this variable directly.
You should change through function'twitter1-icon-mode'")

(make-variable-buffer-local 'twitter1-icon-mode)
(defun twitter1-icon-mode (&optional arg)
  "Toggle display of icon images on timelines.
With a numeric argument, if the argument is positive, turn on
icon mode; otherwise, turn off icon mode."
  (interactive)
  (setq twitter1-icon-mode
	(if (null arg)
	    (not twitter1-icon-mode)
	  (> (prefix-numeric-value arg) 0)))
  (twitter1-update-mode-line)
  (twitter1-render-timeline))

(defvar twitter1-image-data-table
  (make-hash-table :test 'equal))

(defvar twitter1-image-stack nil)
(defvar twitter1-image-type-cache nil)
(defvar twitter1-convert-program (executable-find "convert"))
(defvar twitter1-convert-fix-size 48)
(defvar twitter1-use-convert (not (null twitter1-convert-program))
  "*This variable makes a sense only if `twitter1-convert-fix-size'
is non-nil. If this variable is non-nil, icon images are converted by
invoking \"convert\". Otherwise, cropped images are displayed.")

(defun twitter1-image-type (image-url buffer)
  "Return the type of a given image based on the URL(IMAGE-URL)
and its contents(BUFFER)"
  (let ((type-cache (assoc image-url twitter1-image-type-cache))
	(case-fold-search t))
    (if type-cache
	(cdr type-cache)
      (let ((image-type
	     (cond
	      ((image-type-from-data (buffer-string)))
	      ((executable-find "file")
	       (with-temp-buffer
		 (let ((res-buf (current-buffer)))
		   (save-excursion
		     (set-buffer buffer)
		     (call-process-region (point-min) (point-max)
					  (executable-find "file")
					  nil res-buf nil "-b" "-")))
		 (let ((file-output (buffer-string)))
		   (cond
		    ((string-match "JPEG" file-output) 'jpeg)
		    ((string-match "PNG" file-output) 'png)
		    ((string-match "GIF" file-output) 'gif)
		    ((string-match "bitmap" file-output) 'bitmap)
		    (t nil)))))
	      ((string-match "\\.jpe?g\\(\\?[^/]+\\)?$" image-url) 'jpeg)
	      ((string-match "\\.png\\(\\?[^/]+\\)?$" image-url) 'png)
	      ((string-match "\\.gif\\(\\?[^/]+\\)?$" image-url) 'gif)
	      (t nil))))
	(add-to-list 'twitter1-image-type-cache `(,image-url . ,image-type))
	image-type))))

;;;
;;; functions
;;;

(defun twitter1-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%s" username id))

(defun twitter1-user-agent-default-function ()
  "twitter1 mode default User-Agent function."
  (format "Emacs/%d.%d twitter1-mode/%s"
	  emacs-major-version emacs-minor-version
	  twitter1-mode-version))

(defvar twitter1-sign-simple-string nil)

(defun twitter1-sign-string-default-function ()
  "Tweet append sign string:simple "
  (if twitter1-sign-simple-string
      (format " [%s]" twitter1-sign-simple-string)
    ""))

(defvar twitter1-user-agent-function 'twitter1-user-agent-default-function)
(defvar twitter1-sign-string-function 'twitter1-sign-string-default-function)

(defun twitter1-user-agent ()
  "Return User-Agent header string."
  (funcall twitter1-user-agent-function))

(defun twitter1-sign-string ()
  "Return Tweet sign string."
  (funcall twitter1-sign-string-function))

;;;
;;; Utility functions
;;;

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
(defun twitter1-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      (ucs-to-char num)
    (decode-char 'ucs num)))

(defun twitter1-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))

(defun twitter1-local-strftime (fmt string)
  (twitter1-setftime fmt string nil))
(defun twitter1-global-strftime (fmt string)
  (twitter1-setftime fmt string t))

;;;
;;; Utility functions for portability
;;;

(defun twitter1-remove-duplicates (list)
  "Return a copy of LIST with all duplicate elements removed.
This is non-destructive version of `delete-dups' which is not
defined in Emacs21."
  (if (< emacs-major-version 22)
      (let ((rest list)
            (result nil))
        (while rest
          (unless (member (car rest) result)
            (setq result (cons (car rest) result)))
          (setq rest (cdr rest)))
        (nreverse result))
    (delete-dups (copy-sequence list))))

(defun twitter1-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
"Read a string in the minibuffer, with completion.
This is a modified version of `completing-read' and accepts candidates
as a list of a string on Emacs21."
  ;; completing-read() of Emacs21 does not accepts candidates as
  ;; a list. Candidates must be given as an alist.
  (let* ((collection (twitter1-remove-duplicates collection))
         (collection
          (if (and (< emacs-major-version 22)
                   (listp collection)
                   (stringp (car collection)))
              (mapcar (lambda (x) (cons x nil)) collection)
            collection)))
    (completing-read prompt collection predicate require-match
                     initial-input hist def inherit-input-method)))

;;;
;;; Timeline spec functions
;;;

;;; Timeline spec as S-expression
;;; - (user USER): timeline of the user whose name is USER. USER is a string.
;;; - (list USER LIST):
;;;     the list LIST of the user USER. LIST and USER are strings.
;;;
;;; - (direct-messages): received direct messages.
;;; - (direct-messages-sent): sent direct messages.
;;; - (friends): friends timeline.
;;; - (home): home timeline.
;;; - (mentions): mentions timeline.
;;;     mentions (status containing @username) for the authenticating user.
;;; - (public): public timeline.
;;; - (replies): replies.
;;; - (retweeted_by_me): retweets posted by the authenticating user.
;;; - (retweeted_to_me): retweets posted by the authenticating user's friends.
;;; - (retweets_of_me):
;;;     tweets of the authenticated user that have been retweeted by others.
;;;
;;; - (search STRING): the result of searching with query STRING.
;;; - (merge SPEC1 SPEC2 ...): result of merging timelines SPEC1 SPEC2 ...
;;; - (filter REGEXP SPEC): timeline filtered with REGEXP.
;;;

;;; Timeline spec string
;;;
;;; SPEC ::= PRIMARY | COMPOSITE
;;; PRIMARY ::= USER | LIST | DIRECT-MESSSAGES | DIRECT-MESSSAGES-SENT
;;;             | FRIENDS | HOME | MENTIONS | PUBLIC | REPLIES
;;;             | RETWEETED_BY_ME | RETWEETED_TO_ME | RETWEETS_OF_ME
;;; COMPOSITE ::= MERGE | FILTER
;;;
;;; USER ::= /[a-zA-Z0-9_-]+/
;;; LIST ::= USER "/" LISTNAME
;;; LISTNAME ::= /[a-zA-Z0-9_-]+/
;;; DIRECT-MESSSAGES ::= ":direct-messages"
;;; DIRECT-MESSSAGES-SENT ::= ":direct-messages-sent"
;;; FRIENDS ::= ":friends"
;;; HOME ::= ":home" | "~"
;;; MENTIONS ::= ":mentions"
;;; PUBLIC ::= ":public"
;;; REPLIES ::= ":replies" | "@"
;;; RETWEETED_BY_ME ::= ":retweeted_by_me"
;;; RETWEETED_TO_ME ::= ":retweeted_to_me"
;;; RETWEETS_OF_ME ::= ":retweets_of_me"
;;;
;;; MERGE ::= "(" MERGED_SPECS ")"
;;; MERGED_SPECS ::= SPEC | SPEC "+" MERGED_SPECS
;;; FILTER ::= ":filter/" REGEXP "/" SPEC
;;;

(defun twitter1-timeline-spec-to-string (timeline-spec &optional shorten)
  "Convert TIMELINE-SPEC into a string.
If SHORTEN is non-nil, the abbreviated expression will be used."
  (let ((type (car timeline-spec))
	(value (cdr timeline-spec)))
    (cond
     ;; user
     ((eq type 'user) (car value))
     ;; list
     ((eq type 'list) (concat (car value) "/" (cadr value)))
     ;; simple
     ((eq type 'direct-messages) ":direct-messages")
     ((eq type 'direct-messages-sent) ":direct-messages-sent")
     ((eq type 'friends) ":friends")
     ((eq type 'home) (if shorten "~" ":home"))
     ((eq type 'mentions) ":mentions")
     ((eq type 'public) ":public")
     ((eq type 'replies) (if shorten "@" ":replies"))
     ((eq type 'retweeted_by_me) ":retweeted_by_me")
     ((eq type 'retweeted_to_me) ":retweeted_to_me")
     ((eq type 'retweets_of_me) ":retweets_of_me")
     ;; composite
     ((eq type 'filter)
      (let ((regexp (car value))
	    (spec (cadr value)))
	(concat ":filter/"
		(replace-regexp-in-string "/" "\\/" regexp nil t)
		"/"
		(twitter1-timeline-spec-to-string spec))))
     ((eq type 'merge)
      (concat "("
	      (mapconcat 'twitter1-timeline-spec-to-string value "+" )
	      ")"))
     (t
      nil))))

(defun twitter1-extract-timeline-spec (str &optional unresolved-aliases)
  "Extract one timeline spec from STR.
Return cons of the spec and the rest string."
  (cond
   ((string-match "^\\([a-zA-Z0-9_-]+\\)/\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (listname (match-string 2 str))
	  (rest (substring str (match-end 0))))
      `((list ,user ,listname) . ,rest)))
   ((string-match "^\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (rest (substring str (match-end 0))))
      `((user ,user) . ,rest)))
   ((string-match "^~" str)
    `((home) . ,(substring str (match-end 0))))
   ((string-match "^@" str)
    `((replies) . ,(substring str (match-end 0))))
   ((string-match "^:\\([a-z_-]+\\)" str)
    (let ((type (match-string 1 str))
	  (following (substring str (match-end 0)))
	  (alist '(("direct-messages" . direct-messages)
		   ("direct-messages-sent" . direct-messages-sent)
		   ("friends" . friends)
		   ("home" . home)
		   ("mentions" . mentions)
		   ("public" . public)
		   ("replies" . replies)
		   ("retweeted_by_me" . retweeted_by_me)
		   ("retweeted_to_me" . retweeted_to_me)
		   ("retweets_of_me" . retweets_of_me))))
      (cond
       ((assoc type alist)
	(let ((first-spec (list (cdr (assoc type alist)))))
	  (cons first-spec following)))
       ((string= type "filter")
	(if (string-match "^:filter/\\(.*?[^\\]\\)??/" str)
	    (let* ((escaped-regexp (or (match-string 1 str) ""))
		   (regexp
		    (replace-regexp-in-string "\\\\/" "/"
					      escaped-regexp nil t))
		   (following (substring str (match-end 0)))
		   (pair (twitter1-extract-timeline-spec
			  following unresolved-aliases))
		   (spec (car pair))
		   (rest (cdr pair)))
	      `((filter ,regexp ,spec) . ,rest))
	  (error "\"%s\" has no valid regexp" str)
	  nil))
       (t
	nil))))
   ((string-match "^\\$\\([a-zA-Z0-9_-]+\\)" str)
    (let* ((name (match-string 1 str))
	   (rest (substring str (match-end 1)))
	   (value (cdr-safe (assoc name twitter1-timeline-spec-alias))))
      (if (member name unresolved-aliases)
	  (error "Alias \"%s\" includes a recursive reference" name)
	(if value
	    (twitter1-extract-timeline-spec
	     (concat value rest)
	     (cons name unresolved-aliases))
	  (error "Alias \"%s\" is undefined" name)))))
   ((string-match "^(" str)
    (let* ((rest (concat "+" (substring str (match-end 0))))
	   (result '()))
      (while (and rest (string-match "^\\+" rest))
	(let* ((spec-string (substring rest (match-end 0)))
	       (pair (twitter1-extract-timeline-spec
		      spec-string unresolved-aliases))
	       (spec (car pair))
	       (next-rest (cdr pair)))
	  (setq result (cons spec result))
	  (setq rest next-rest)))
      (if (and rest (string-match "^)" rest))
	  (let ((spec-list
		 (apply 'append
			(mapcar (lambda (x) (if (eq 'merge (car x))
						(cdr x)
					      (list x)))
				(reverse result)))))
	    (if (= 1 (length spec-list))
		`(,(car spec-list) . ,(substring rest 1))
	      `((merge ,@spec-list) . ,(substring rest 1))))
	(if rest
	    (error "\"%s\" lacks a closing parenthesis" str))
	nil)))
   (t
    nil)
   ))

(defun twitter1-string-to-timeline-spec (spec-str)
  "Convert STR into a timeline spec.
Return nil if STR is invalid as a timeline spec."
  (let ((result-pair (twitter1-extract-timeline-spec spec-str)))
    (if (and result-pair (string= "" (cdr result-pair)))
	(car result-pair)
      nil)))

(defun twitter1-timeline-spec-primary-p (spec)
  "Return non-nil if SPEC is a primary timeline spec.
`primary' means that the spec is not a composite timeline spec such as
`filter' and `merge'."
  (let ((primary-spec-types
	 '(user list
		direct-messages direct-messages-sent
		friends home mentions public replies
		retweeted_by_me retweeted_to_me retweets_of_me))
	(type (car spec)))
    (memq type primary-spec-types)))

(defun twitter1-equal-string-as-timeline (spec-str1 spec-str2)
  "Return non-nil if SPEC-STR1 equals SPEC-STR2 as a timeline spec."
  (if (and (stringp spec-str1) (stringp spec-str2))
      (let ((spec1 (twitter1-string-to-timeline-spec spec-str1))
	    (spec2 (twitter1-string-to-timeline-spec spec-str2)))
	(equal spec1 spec2))
    nil))

(defun twitter1-timeline-spec-to-host-method (spec)
  (if (twitter1-timeline-spec-primary-p spec)
      (let ((type (car spec))
	    (value (cdr spec)))
	(cond
	 ((eq type 'user)
	  (let ((username (car value)))
	    `("twitter.com" ,(concat "statuses/user_timeline/" username))))
	 ((eq type 'list)
	  (let ((username (car value))
		(list-name (cadr value)))
	    `("api.twitter.com"
	      ,(concat "1/" username "/lists/" list-name "/statuses" ))))
	 ((or (eq type 'direct-messages)
	      (eq type 'direct-messages-sent))
	  (error "%s has not been supported yet" type))
	 ((eq type 'friends)
	  '("twitter.com" "statuses/friends_timeline"))
	 ((eq type 'home)
	  '("api.twitter.com" "1/statuses/home_timeline"))
	 ((eq type 'mentions)
	  '("twitter.com" "statuses/mentions"))
	 ((eq type 'public)
	  '("twitter.com" "statuses/public_timeline"))
	 ((eq type 'replies)
	  '("twitter.com" "statuses/replies"))
	 ((eq type 'retweeted_by_me)
	  '("api.twitter.com" "1/statuses/retweeted_by_me"))
	 ((eq type 'retweeted_to_me)
	  '("api.twitter.com" "1/statuses/retweeted_to_me"))
	 ((eq type 'retweets_of_me)
	  '("api.twitter.com" "1/statuses/retweets_of_me"))
	 (t
	  (error "Invalid timeline spec")
	  nil)))
    nil))

(defun twitter1-host-method-to-timeline-spec (host method)
  (cond
   ((or (not (stringp host)) (not (stringp method))) nil)
   ((string= host "twitter.com")
    (cond
     ((string= method "statuses/friends_timeline") '(friends))
     ((string= method "statuses/mentions") '(mentions))
     ((string= method "statuses/replies") '(replies))
     ((string= method "statuses/public_timeline") '(public_timeline))
     ((string= method "statuses/user_timeline")
      `(user ,(twitter1-get-username)))
     ((string-match "^statuses/user_timeline/\\(.+\\)$" method)
      `(user ,(match-string-no-properties 1 method)))
     (t nil)))
   ((string= host "api.twitter.com")
    (cond
     ((string= method "1/statuses/home_timeline") '(home))
     ((string= method "1/statuses/retweeted_by_me") '(retweeted_by_me))
     ((string= method "1/statuses/retweeted_to_me") '(retweeted_to_me))
     ((string= method "1/statuses/retweets_of_me") '(retweets_of_me))
     ((string-match "^1/\\([^/]+\\)/lists/\\([^/]+\\)/statuses" method)
      (let ((username (match-string-no-properties 1 method))
	    (listname (match-string-no-properties 2 method)))
	`(list ,username ,listname)))
     (t nil)))
   (t nil)))

(defun twitter1-add-timeline-history (&optional timeline-spec)
  (let* ((spec-string
	  (if timeline-spec
	      (twitter1-timeline-spec-to-string timeline-spec t)
	    twitter1-last-retrieved-timeline-spec-string)))
    (when spec-string
      (when (or (null twitter1-timeline-history)
		(not (string= spec-string (car twitter1-timeline-history))))
	(if (functionp 'add-to-history)
	    (add-to-history 'twitter1-timeline-history spec-string)
	  (setq twitter1-timeline-history
		(cons spec-string twitter1-timeline-history)))))))

;;;
;;; Debug mode
;;;

(defvar twitter1-debug-mode nil)
(defvar twitter1-debug-buffer "*debug*")

(defun twitter1-debug-buffer ()
  (twitter1-get-or-generate-buffer twitter1-debug-buffer))

(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twitter1-debug-mode
	   (with-current-buffer (twitter1-debug-buffer)
	     (insert "[debug] ")
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun debug-printf (fmt &rest args)
  (when twitter1-debug-mode
    (with-current-buffer (twitter1-debug-buffer)
      (insert (concat "[debug] " (apply 'format fmt args)))
      (newline))))

(defun twitter1-debug-mode ()
  (interactive)
  (setq twitter1-debug-mode
	(not twitter1-debug-mode))
  (message (if twitter1-debug-mode "debug mode:on" "debug mode:off")))

;;;
;;; keymap
;;;

(if twitter1-mode-map
    (let ((km twitter1-mode-map))
      (define-key km "\C-c\C-f" 'twitter1-friends-timeline)
      (define-key km "e" 'twitter1-friends-timeline)
      (define-key km "\C-c\C-r" 'twitter1-replies-timeline)
      (define-key km "r" 'twitter1-replies-timeline)
      (define-key km "\C-c\C-g" 'twitter1-public-timeline)
      (define-key km "\C-c\C-u" 'twitter1-user-timeline)
      (define-key km "u" 'twitter1-user-timeline)
      (define-key km "\C-c\C-s" 'twitter1-update-status-interactive)
      (define-key km "w" 'twitter1-update-status-interactive)
      (define-key km "\C-c\C-d" 'twitter1-erase-old-statuses)
      (define-key km "\C-c\C-m" 'twitter1-retweet)
      (define-key km "\C-c\C-h" 'twitter1-set-current-hashtag)
      (define-key km "\C-m" 'twitter1-enter)
      (define-key km "\C-c\C-l" 'twitter1-update-lambda)
      (define-key km [mouse-1] 'twitter1-click)
      (define-key km "\C-c\C-v" 'twitter1-view-user-page)
      (define-key km "g" 'twitter1-current-timeline)
      (define-key km "d" 'twitter1-direct-message)
      (define-key km "v" 'twitter1-other-user-timeline)
      (define-key km "V" 'twitter1-visit-timeline)
      (define-key km "L" 'twitter1-other-user-list-interactive)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twitter1-goto-next-status)
      (define-key km "k" 'twitter1-goto-previous-status)
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
      ;; (define-key km "n" 'twitter1-goto-next-status-of-user)
      ;; (define-key km "p" 'twitter1-goto-previous-status-of-user)
      (define-key km "n" 'windmove-down)
      (define-key km "p" 'windmove-up)
      (define-key km "\C-i" 'twitter1-goto-next-thing)
      (define-key km "\M-\C-i" 'twitter1-goto-previous-thing)
      (define-key km [backtab] 'twitter1-goto-previous-thing)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'twitter1-icon-mode)
      (define-key km "s" 'twitter1-scroll-mode)
      (define-key km "t" 'twitter1-toggle-proxy)
      (define-key km "\C-c\C-p" 'twitter1-toggle-proxy)
      (define-key km "\C-c\C-q" 'twitter1-suspend)
      nil))

(defun twitter1-keybind-message ()
  (let ((important-commands
	 '(("Timeline" . twitter1-friends-timeline)
	   ("Replies" . twitter1-replies-timeline)
	   ("Update status" . twitter1-update-status-interactive)
	   ("Next" . twitter1-goto-next-status)
	   ("Prev" . twitter1-goto-previous-status))))
    (mapconcat (lambda (command-spec)
		 (let ((descr (car command-spec))
		       (command (cdr command-spec)))
		   (format "%s: %s" descr (key-description
					   (where-is-internal
					    command
					    overriding-local-map t)))))
	       important-commands ", ")))

;; (run-with-idle-timer
;;  0.1 t
;;  '(lambda ()
;;     (when (equal (buffer-name (current-buffer)) twitter1-buffer)
;;       (message (twitter1-keybind-message)))))

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
;;   (add-to-list 'minor-mode-alist '(twitter1-icon-mode " tw-icon"))
;;   (add-to-list 'minor-mode-alist '(twitter1-scroll-mode " tw-scroll"))
;;   (add-to-list 'minor-mode-alist '(twitter1-jojo-mode " tw-jojo"))
  (setq twitter1-username-active twitter1-username)
  (setq twitter1-password-active twitter1-password)
  (when twitter1-use-convert
    (if (null twitter1-convert-program)
	(setq twitter1-use-convert nil)
      (with-temp-buffer
	(call-process twitter1-convert-program nil (current-buffer) nil
		      "-version")
	(goto-char (point-min))
	(if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick" nil t))
	    (setq twitter1-use-convert nil)))))
  (twitter1-setup-proxy)
  )

(defvar twitter1-mode-string "twitter1-mode")

(defvar twitter1-mode-hook nil
  "twitter1-mode hook.")

(defun twitter1-update-mode-line ()
  "Update mode line"
  (let ((enabled-options nil)
	(spec-string twitter1-last-retrieved-timeline-spec-string))
    (when twitter1-jojo-mode
      (push "jojo" enabled-options))
    (when twitter1-icon-mode
      (push "icon" enabled-options))
    (when twitter1-scroll-mode
      (push "scroll" enabled-options))
    (when twitter1-proxy-use
      (push "proxy" enabled-options))
    (when twitter1-use-ssl
      (push "ssl" enabled-options))
    (setq mode-name
	  (concat twitter1-mode-string
		  (if spec-string
		      (concat " " spec-string)
		    "")
		  (if enabled-options
		      (concat "["
			      (mapconcat 'identity enabled-options ",")
			      "]")
		    ""))))
  (force-mode-line-update)
  )

;;;
;;; Basic HTTP functions
;;;

(defun twitter1-find-curl-program ()
  "Returns an appropriate 'curl' program pathname or nil if not found."
  (or (executable-find "curl")
      (let ((windows-p (find system-type '(windows-nt cygwin)))
	    (curl.exe
	     (expand-file-name
	      "curl.exe"
	      (expand-file-name
	       "win-curl"
	       (file-name-directory (symbol-file 'twit))))))
	(and windows-p
	     (file-exists-p curl.exe) curl.exe))))

(defun twitter1-start-http-session (method headers host port path parameters &optional noninteractive sentinel)
  "
METHOD    : http method
HEADERS   : http request heades in assoc list
HOST      : remote host name
PORT      : destination port number. nil means default port(http: 80, https: 443)
PATH      : http request path
PARAMETERS: http request parameters (query string)
"
  (block nil
    (unless (find method '("POST" "GET") :test 'equal)
      (error "Unknown HTTP method: %s" method))
    (unless (string-match "^/" path)
      (error "Invalid HTTP path: %s" path))

    (unless (assoc "Host" headers)
      (setq headers (cons `("Host" . ,host) headers)))
    (unless (assoc "User-Agent" headers)
      (setq headers (cons `("User-Agent" . ,(twitter1-user-agent))
			  headers)))

    (let ((curl-program nil))
      (when twitter1-use-ssl
	(cond 
	 ((not (setq curl-program (twitter1-find-curl-program)))
	  (if (yes-or-no-p "HTTPS(SSL) is not available because 'cURL' does not exist. Use HTTP instead? ")
	      (progn (setq twitter1-use-ssl nil)
		     (twitter1-update-mode-line))
	    (message "Request canceled")
	    (return)))
	 ((not (with-temp-buffer
		 (call-process curl-program
			       nil (current-buffer) nil
			       "--version")
		 (goto-char (point-min))
		 (search-forward-regexp
		  "^Protocols: .*https" nil t)))
	  (if (yes-or-no-p "HTTPS(SSL) is not available because your 'cURL' cannot use HTTPS. Use HTTP instead? ")
	      (progn (setq twitter1-use-ssl nil)
		     (twitter1-update-mode-line))
	    (message "Request canceled")
	    (return)))))

      (if twitter1-use-ssl
	  (twitter1-start-http-ssl-session
	   curl-program method headers host port path parameters
	   noninteractive sentinel)
	(twitter1-start-http-non-ssl-session
	 method headers host port path parameters
	 noninteractive sentinel)))))

;;; FIXME: file name is hard-coded. More robust way is desired.
(defvar twitter1-cert-file nil)
(defun twitter1-ensure-ca-cert ()
  "Create a CA certificate file if it does not exist, and return
its file name."
  (if twitter1-cert-file
      twitter1-cert-file
    (let ((file-name (make-temp-file "twmode-cacert")))
      (with-temp-file file-name
	(insert "-----BEGIN CERTIFICATE-----
MIICkDCCAfmgAwIBAgIBATANBgkqhkiG9w0BAQQFADBaMQswCQYDVQQGEwJVUzEc
MBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5jLjEtMCsGA1UEAxMkRXF1aWZheCBT
ZWN1cmUgR2xvYmFsIGVCdXNpbmVzcyBDQS0xMB4XDTk5MDYyMTA0MDAwMFoXDTIw
MDYyMTA0MDAwMFowWjELMAkGA1UEBhMCVVMxHDAaBgNVBAoTE0VxdWlmYXggU2Vj
dXJlIEluYy4xLTArBgNVBAMTJEVxdWlmYXggU2VjdXJlIEdsb2JhbCBlQnVzaW5l
c3MgQ0EtMTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAuucXkAJlsTRVPEnC
UdXfp9E3j9HngXNBUmCbnaEXJnitx7HoJpQytd4zjTov2/KaelpzmKNc6fuKcxtc
58O/gGzNqfTWK8D3+ZmqY6KxRwIP1ORROhI8bIpaVIRw28HFkM9yRcuoWcDNM50/
o5brhTMhHD4ePmBudpxnhcXIw2ECAwEAAaNmMGQwEQYJYIZIAYb4QgEBBAQDAgAH
MA8GA1UdEwEB/wQFMAMBAf8wHwYDVR0jBBgwFoAUvqigdHJQa0S3ySPY+6j/s1dr
aGwwHQYDVR0OBBYEFL6ooHRyUGtEt8kj2Puo/7NXa2hsMA0GCSqGSIb3DQEBBAUA
A4GBADDiAVGqx+pf2rnQZQ8w1j7aDRRJbpGTJxQx78T3LUX47Me/okENI7SS+RkA
Z70Br83gcfxaz2TE4JaY0KNA4gGK7ycH8WUBikQtBmV1UsCGECAhX2xrD2yuCRyv
8qIYNMR1pHMc8Y3c7635s3a0kr/clRAevsvIO1qEYBlWlKlV
-----END CERTIFICATE-----"))
      (setq twitter1-cert-file file-name))))

(defun twitter1-start-http-ssl-session (curl-program method headers host port path parameters &optional noninteractive sentinel)
  ;; TODO: use curl
  (let* ((request (twitter1-make-http-request
		   method headers host port path parameters))
	 (headers (if (assoc "Expect" headers)
		      headers
		    (cons '("Expect" . "") headers)))
	 (curl-args
	  `("--include" "--silent"
	    ,@(mapcan (lambda (pair)
			(list "-H"
			      (format "%s: %s"
				      (car pair) (cdr pair))))
		      headers)
	    "--cacert"
	    ,(twitter1-ensure-ca-cert))))
    (when twitter1-proxy-use
      (nconc curl-args `("-x" ,(format "%s:%s" twitter1-proxy-server
					 twitter1-proxy-port)))
      (when (and twitter1-proxy-user
		 twitter1-proxy-password)
	(nconc curl-args `("-U" ,(format "%s:%s" twitter1-proxy-user
					   twitter1-proxy-password)))))

    (flet ((request (key) (funcall request key)))
      (nconc curl-args `(,(if parameters
			      (concat (request :uri) "?"
				      (request :query-string))
			    (request :uri))))
      (when (string-equal "POST" method)
	(nconc curl-args 
	       `(,@(mapcan (lambda (pair)
			     (list
			      "-d"
			      (format "%s=%s"
				      (twitter1-percent-encode
				       (car pair))
				      (twitter1-percent-encode
				       (cdr pair)))))
			   parameters)))))
    (debug-print curl-args)
    (lexical-let ((temp-buffer
		   (generate-new-buffer "*twmode-http-buffer*"))
		  (noninteractive noninteractive)
		  (sentinel sentinel))
      (let ((curl-process
	     (apply 'start-process
		    "*twmode-curl*"
		    temp-buffer
		    curl-program
		    curl-args)))
	(set-process-sentinel
	 curl-process
	 (lambda (&rest args)
	   (apply sentinel temp-buffer noninteractive args))))))
  )

;; TODO: proxy
(defun twitter1-start-http-non-ssl-session (method headers host port path parameters &optional noninteractive sentinel)
  (let ((request (twitter1-make-http-request
		  method headers host port path parameters)))
    (flet ((request (key) (funcall request key)))
      (let* ((request-str
	      (format "%s %s%s HTTP/1.1\r\n%s\r\n\r\n"
		      (request :method)
		      (request :uri)
		      (if parameters
			  (concat "?" (request :query-string))
			"")
		      (request :headers-string)))
	     (server (if twitter1-proxy-use
			 twitter1-proxy-server
		       (request :host)))
	     (port (if twitter1-proxy-use
		       twitter1-proxy-port
		     (request :port)))
	     (temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	     (proc (open-network-stream
		    "network-connection-process" temp-buffer server port))
	     )
	(lexical-let ((temp-buffer temp-buffer)
		      (sentinel sentinel)
		      (noninteractive noninteractive))
	  (set-process-sentinel
	   proc
	   (lambda (&rest args)
	     (apply sentinel temp-buffer noninteractive args))))
	(debug-print request-str)
	(process-send-string proc request-str))))
  )

;;; TODO: proxy
(defun twitter1-make-http-request (method headers host port path parameters)
  "Returns an anonymous function, which holds request data.

A returned function, say REQUEST, is used in this way:
  (funcall REQUEST :schema) ; => \"http\" or \"https\"
  (funcall REQUEST :uri) ; => \"http://twitter.com/user_timeline\"
  (funcall REQUEST :query-string) ; => \"status=hello+twitter&source=twmode\"
  ...

Available keywords:
  :method
  :host
  :port
  :headers
  :headers-string
  :schema
  :uri
  :query-string
  "
  (let* ((schema (if twitter1-use-ssl "https" "http"))
	 (default-port (if twitter1-use-ssl 443 80))
	 (port (if port port default-port))
	 (headers-string
	  (mapconcat (lambda (pair)
		       (format "%s: %s" (car pair) (cdr pair)))
		     headers "\r\n"))
	 (uri (format "%s://%s%s%s"
		      schema
		      host
		      (if port
			  (if (equal port default-port)
			      ""
			    (format ":%s" port))
			"")
		      path))
	 (query-string
	  (mapconcat (lambda (pair)
		       (format
			"%s=%s"
			(twitter1-percent-encode (car pair))
			(twitter1-percent-encode (cdr pair))))
		     parameters
		     "&"))
	 )
    (lexical-let ((data `((:method . ,method)
			  (:host . ,host)
			  (:port . ,port)
			  (:headers . ,headers)
			  (:headers-string . ,headers-string)
			  (:schema . ,schema)
			  (:uri . ,uri)
			  (:query-string . ,query-string)
			  )))
      (lambda (key)
	(let ((pair (assoc key data)))
	  (if pair (cdr pair)
	    (error "No such key in HTTP request data: %s" key))))
      )))

(defun twitter1-http-application-headers (&optional method headers)
  "Retuns an assoc list of HTTP headers for twitter1-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twitter1-user-agent)) headers)
    (push (cons "Authorization"
		(concat "Basic "
			(base64-encode-string
			 (concat
			  (twitter1-get-username)
			  ":"
			  (twitter1-get-password)))))
	  headers)
    (when (string-equal "GET" method)
      (push (cons "Accept"
		  (concat
		   "text/xml"
		   ",application/xml"
		   ",application/xhtml+xml"
		   ",application/html;q=0.9"
		   ",text/plain;q=0.8"
		   ",image/png,*/*;q=0.5"))
	    headers)
      (push (cons "Accept-Charset" "utf-8;q=0.7,*;q=0.7")
	    headers))
    (when (string-equal "POST" method)
      (push (cons "Content-Length" "0") headers)
      (push (cons "Content-Type" "text/plain") headers))
    (when twitter1-proxy-use
      (when twitter1-proxy-keep-alive
	(push (cons "Proxy-Connection" "Keep-Alive")
	      headers))
      (when (and twitter1-proxy-user
		 twitter1-proxy-password)
	(push (cons "Proxy-Authorization"
		    (concat
		     "Basic "
		     (base64-encode-string
		      (concat
		       twitter1-proxy-user
		       ":"
		       twitter1-proxy-password))))
	      headers)))
    headers
    ))

(defun twitter1-http-get (host method &optional noninteractive parameters sentinel)
  (if (null sentinel)
      (setq sentinel 'twitter1-http-get-default-sentinel))

  (twitter1-start-http-session
   "GET" (twitter1-http-application-headers "GET")
   host nil (concat "/" method ".xml") parameters noninteractive sentinel))

(defun twitter1-created-at-to-seconds (created-at)
  (let ((encoded-time (apply 'encode-time (parse-time-string created-at))))
    (+ (* (car encoded-time) 65536)
       (cadr encoded-time))))

(defun twitter1-http-get-default-sentinel (temp-buffer noninteractive proc stat &optional suc-msg)
  (debug-printf "get-default-sentinel: proc=%s stat=%s" proc stat)
  (unwind-protect
      (let ((header (twitter1-get-response-header temp-buffer))
	    (body (twitter1-get-response-body temp-buffer))
	    (status nil))
	(if (string-match "HTTP/1\.[01] \\([a-zA-Z0-9 ]+\\)\r?\n" header)
	    (when body
	      (setq status (match-string-no-properties 1 header))
	      (case-string
	       status
	       (("200 OK")
		(setq twitter1-new-tweets-count
		      (count t (mapcar
				#'twitter1-cache-status-datum
				(reverse (twitter1-xmltree-to-status
					  body)))))
		(setq twitter1-timeline-data
		      (sort twitter1-timeline-data
			    (lambda (status1 status2)
			      (let ((created-at1
				     (twitter1-created-at-to-seconds
				      (cdr (assoc 'created-at status1))))
				    (created-at2
				     (twitter1-created-at-to-seconds
				      (cdr (assoc 'created-at status2)))))
				(> created-at1 created-at2)))))
		(if (and (> twitter1-new-tweets-count 0)
			 noninteractive)
		    (run-hooks 'twitter1-new-tweets-hook))
		(setq twitter1-last-retrieved-timeline-spec-string
		      twitter1-last-requested-timeline-spec-string)
		(twitter1-render-timeline)
		(twitter1-add-timeline-history)
		(when twitter1-notify-successful-http-get
		  ))
	       (t (message status))))
	  (message "Failure: Bad http response.")))
    ;; unwindforms
    (when (and (not twitter1-debug-mode) (buffer-live-p temp-buffer))
      (kill-buffer temp-buffer)))
  )

;; XXX: this is a preliminary implementation because we should parse
;; xmltree in the function.
(defun twitter1-http-get-list-index-sentinel (temp-buffer noninteractive proc stat &optional suc-msg)
  (debug-printf "get-list-index-sentinel: proc=%s stat=%s" proc stat)
  (unwind-protect
      (let ((header (twitter1-get-response-header temp-buffer)))
	(if (not (string-match "HTTP/1\.[01] \\([a-zA-Z0-9 ]+\\)\r?\n" header))
	    (setq twitter1-list-index-retrieved "Failure: Bad http response.")
	  (let ((status (match-string-no-properties 1 header))
		(indexes nil))
	    (if (not (string-match "\r?\nLast-Modified: " header))
		(setq twitter1-list-index-retrieved
		      (concat status ", but no contents."))
	      (case-string
	       status
	       (("200 OK")
		(with-current-buffer temp-buffer
		  (save-excursion
		    (goto-char (point-min))
		    (if (search-forward-regexp "\r?\n\r?\n" nil t)
			(while (re-search-forward
				"<slug>\\([-a-zA-Z0-9_]+\\)</slug>" nil t)
			  (push (match-string 1) indexes)))
		    (if indexes
			(setq twitter1-list-index-retrieved indexes)
		      (setq twitter1-list-index-retrieved "")))))
	       (t
		(setq twitter1-list-index-retrieved status)))))))
    ;; unwindforms
    (when (and (not twitter1-debug-mode) (buffer-live-p temp-buffer))
      (kill-buffer temp-buffer)))
  )

(defun twitter1-http-post (host method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com (or api.twitter.com)

HOST is hostname of remote side, twitter.com or api.twitter.com.
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel)
      (setq sentinel 'twitter1-http-post-default-sentinel))

  (twitter1-start-http-session
   "POST" (twitter1-http-application-headers "POST")
   host nil (concat "/" method ".xml") parameters noninteractive sentinel))

(defun twitter1-http-post-default-sentinel (temp-buffer noninteractive proc stat &optional suc-msg)
  (debug-printf "post-default-sentinel: proc=%s stat=%s" proc stat)
  (unwind-protect
      (let ((header (twitter1-get-response-header temp-buffer))
	    ;; (body (twitter1-get-response-body temp-buffer)) not used now.
	    (status nil))
	(if (string-match "HTTP/1\.[01] \\([a-zA-Z0-9 ]+\\)\r?\n" header)
	    (setq status (match-string-no-properties 1 header))
	  (setq status
		(progn (string-match "^\\([^\r\n]+\\)\r?\n" header)
		       (match-string-no-properties 1 header))))
	(case-string status
		     (("200 OK")
		       )
		     (t (message "Response status code: %s" status)))
	)
    ;; unwindforms
    (when (and (not twitter1-debug-mode) (buffer-live-p temp-buffer))
      (kill-buffer temp-buffer)))
  )

(defun twitter1-get-response-header (buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer which contains the HTTP response."
  (if (stringp buffer)
      (setq buffer (get-buffer buffer)))

  ;; FIXME:
  ;; curl prints HTTP proxy response header, so strip it
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp
	     "HTTP/1\\.[01] 200 Connection established\r\n\r\n" nil t)
	(delete-region (point-min) (point)))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (buffer-substring (point-min) (match-end 0))
	(error "Failure: invalid HTTP response")))))

(defun twitter1-get-response-body (buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a
XML tree as list. Return nil when parse failed.
`buffer' may be a buffer or the name of an existing buffer. "
  (if (stringp buffer)
      (setq buffer (get-buffer buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (let ((start (match-end 0)))
	    (condition-case get-error ;; to guard when `xml-parse-region' failed.
		(xml-parse-region start (point-max))
	      (error (message "Failure: %s" get-error)
		     nil)))
	(error "Failure: invalid HTTP response"))
      )))

(defun twitter1-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twitter1-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twitter1-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (string= id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (if twitter1-jojo-mode
	      (twitter1-update-jojo (cdr (assq 'user-screen-name
						 status-datum))
				      (cdr (assq 'text status-datum))))
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twitter1-status-to-status-datum (status)
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
	   regex-index
	   (retweeted-status-data (cddr (assq 'retweeted_status status-data)))
	   original-user-name
	   original-user-screen-name)

      ;; save original status and adjust data if status was retweeted
      (when (and retweeted-status-data twitter1-use-native-retweet)
	(setq original-user-screen-name (twitter1-decode-html-entities
					 (assq-get 'screen_name user-data))
	      original-user-name (twitter1-decode-html-entities
				  (assq-get 'name user-data)))
	(setq status-data retweeted-status-data
	      user-data (cddr (assq 'user retweeted-status-data))))

      (setq id (assq-get 'id status-data))
      (setq text (twitter1-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twitter1-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq in-reply-to-status-id
	    (twitter1-decode-html-entities
	     (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
	    (twitter1-decode-html-entities
	     (assq-get 'in_reply_to_screen_name status-data)))
      (setq user-id (assq-get 'id user-data))
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
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twitter1-username-face)
       user-screen-name)

      ;; make screen-name in text clickable
      (let ((pos 0))
	(block nil
	  (while (string-match "@\\([_a-zA-Z0-9]+\\)" text pos)
	    (let ((next-pos (match-end 0))
		  (screen-name (match-string 1 text)))
	      (when (eq next-pos pos)
		(return nil))

	      (add-text-properties
	       (match-beginning 1) (match-end 1)
	       `(screen-name-in-text ,screen-name) text)
	      (add-text-properties
	       (match-beginning 1) (match-end 1)
	       `(mouse-face highlight
			    uri ,(concat "http://twitter.com/" screen-name)
			    face twitter1-username-face)
	       text)

	      (setq pos next-pos)))))

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
		   uri ,(concat "http://twitter.com/" screen-name)
		   uri-in-text ,(concat "http://twitter.com/" screen-name))
	       `(mouse-face highlight
			    face twitter1-uri-face
			    uri ,uri
			    uri-in-text ,uri))
	     text))
	  (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>" source)
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
      (when (or (null twitter1-timeline-last-update)
                (< (twitter1-created-at-to-seconds
                    twitter1-timeline-last-update)
                   (twitter1-created-at-to-seconds created-at)))
        (setq twitter1-timeline-last-update created-at))

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
	    user-protected
	    original-user-name
	    original-user-screen-name)))))

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
      (t (format "%%%02x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter1-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?Z))
      (and (<= ?a ch) (<= ch ?z))
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

;;;
;;; display functions
;;;

(defun twitter1-render-timeline ()
  (with-current-buffer (twitter1-buffer)
    (let ((point (point))
	  (end (point-max))
	  (fill-column (- (window-width) 4)))
      (twitter1-update-mode-line)
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (twitter1-format-status
		       status twitter1-status-format))
	      (insert "\n"))
	    twitter1-timeline-data)
      (if (and twitter1-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twitter1-scroll-mode (- (point-max) end) 0))))
    ))

(defun twitter1-make-display-spec-for-icon (image-url)
  "Return the specification for `display' text property, which
limits the size of an icon image IMAGE-URL up to FIXED-LENGTH. If
the type of the image is not supported, nil is returned.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let* ((image-data (twitter1-retrieve-image image-url))
	 (image-spec
	  `(image :type ,(car image-data)
		  :data ,(cdr image-data))))
    (if (not (image-type-available-p (car image-data)))
	nil
      (if (and twitter1-convert-fix-size (not twitter1-use-convert))
	  (let* ((size (if (cdr image-data)
			   (image-size image-spec t)
			 '(48 . 48)))
		 (width (car size))
		 (height (cdr size))
		 (fixed-length twitter1-convert-fix-size)
		 (half-fixed-length (/ fixed-length 2))
		 (slice-spec
		  (if (or (< fixed-length width) (< fixed-length height))
		      `(slice ,(max 0 (- (/ width 2) half-fixed-length))
			      ,(max 0 (- (/ height 2) half-fixed-length))
			      ,fixed-length ,fixed-length)
		    `(slice 0 0 ,fixed-length ,fixed-length))))
	    `(display (,image-spec ,slice-spec)))
	`(display ,image-spec)))))

(defun twitter1-format-string (string prefix replacement-table)
  "Format STRING according to PREFIX and REPLACEMENT-TABLE.
PREFIX is a regexp. REPLACEMENT-TABLE is a list of (FROM . TO) pairs,
where FROM is a regexp and TO is a string or a 2-parameter function.

The pairs in REPLACEMENT-TABLE are stored in order of precedence.
First, search PREFIX in STRING from left to right.
If PREFIX is found in STRING, try to match the following string with
FROM of each pair in the same order of REPLACEMENT-TABLE. If FROM in
a pair is matched, replace the prefix and the matched string with a
string generated from TO.
If TO is a string, the matched string is replaced with TO.
If TO is a function, the matched string is replaced with the
return value of (funcall TO CONTEXT), where CONTEXT is an alist.
Each element of CONTEXT is (KEY . VALUE) and KEY is one of the
following symbols;
  'following-string  --the matched string following the prefix
  'match-data --the match-data for the regexp FROM.
  'prefix --PREFIX.
  'replacement-table --REPLACEMENT-TABLE.
  'from --FROM.
  'processed-string --the already processed string.
"
  (let ((current-pos 0)
	(result "")
	(case-fold-search nil))
    (while (and (string-match prefix string current-pos)
		(not (eq (match-end 0) current-pos)))
      (let ((found nil)
	    (current-table replacement-table)
	    (next-pos (match-end 0))
	    (matched-string (match-string 0 string))
	    (skipped-string
	     (substring string current-pos (match-beginning 0))))
	(setq result (concat result skipped-string))
	(setq current-pos next-pos)
	(while (and (not (null current-table))
		    (not found))
	  (let ((key (caar current-table))
		(value (cdar current-table))
		(following-string (substring string current-pos))
		(case-fold-search nil))
	    (if (string-match (concat "\\`" key) following-string)
		(let ((next-pos (+ current-pos (match-end 0)))
		      (output
		       (if (stringp value)
			   value
			 (funcall value
				  `((following-string . ,following-string)
				    (match-data . ,(match-data))
				    (prefix . ,prefix)
				    (replacement-table . ,replacement-table)
				    (from . ,key)
				    (processed-string . ,result))))))
		  (setq found t)
		  (setq current-pos next-pos)
		  (setq result (concat result output)))
	      (setq current-table (cdr current-table)))))
	(if (not found)
	    (setq result (concat result matched-string)))))
    (let* ((skipped-string (substring string current-pos)))
      (concat result skipped-string))
    ))

(defun twitter1-format-status (status format-str)
  "Format a string out of a format-str and STATUS.
Specification of format-str is described in the document for the
variable `twitter1-status-format'"
  (flet ((attr (key)
	       (assocref key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url))
		(icon-string "\n  "))
	    (unless (gethash
		     `(,profile-image-url . ,twitter1-convert-fix-size)
		     twitter1-image-data-table)
	      (add-to-list 'twitter1-image-stack profile-image-url))
	    
	    (when (and icon-string twitter1-icon-mode)
	      (let ((display-spec
		     (twitter1-make-display-spec-for-icon profile-image-url)))
		(when display-spec
		  (set-text-properties 1 2 display-spec icon-string)))
	      icon-string)
	    ))
	 (make-string-with-url-property
	  (str url)
	  (let ((result (copy-sequence str)))
	    (add-text-properties
	     0 (length result)
	     `(mouse-face highlight face twitter1-uri-face uri ,url)
	     result)
	    result)))
    (let* ((replace-table
	    `(("%" . "%")
	      ("#" . ,(attr 'id))
	      ("'" . ,(if (string= "true" (attr 'truncated)) "..." ""))
	      ("@" .
	       ,(let* ((created-at
			(apply
			 'encode-time
			 (parse-time-string (attr 'created-at))))
		       (now (current-time))
		       (secs (+ (* (- (car now) (car created-at)) 65536)
				(- (cadr now) (cadr created-at))))
		       (time-string
			(cond
			 ((< secs 5) "less than 5 seconds ago")
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
		       (url
			(twitter1-get-status-url (attr 'user-screen-name)
						   (attr 'id))))
		  ;; make status url clickable
		  (make-string-with-url-property time-string url)))
	      ("C\\({\\([^}]*\\)}\\)?" .
	       (lambda (context)
		 (let ((str (cdr (assq 'following-string context)))
		       (match-data (cdr (assq 'match-data context))))
		   (let* ((time-format
			   (or (match-string 2 str) "%H:%M:%S"))
			  (created-at
			   (apply 'encode-time
				  (parse-time-string (attr 'created-at)))))
		     (format-time-string time-format created-at)))))
	      ("c" . ,(attr 'created-at))
	      ("d" . ,(attr 'user-description))
	      ("FILL{\\(.*?[^%]\\)}" .
	       ,(lambda (context)
		  (let* ((str (cdr (assq 'following-string context)))
			 (match-data (cdr (assq 'match-data context)))
			 (from (cdr (assq 'from context)))
			 (prefix (cdr (assq 'prefix context)))
			 (table (cdr (assq 'replacement-table context)))
			 (mod-table
			  (cons '("}" . "}")
				(delq (assq from table) table))))
		    (store-match-data match-data)
		    (let* ((formatted-str
			    (twitter1-format-string
			     (match-string 1 str) prefix mod-table)))
		      (with-temp-buffer
			(insert formatted-str)
			(fill-region-as-paragraph (point-min) (point-max))
			(buffer-substring (point-min) (point-max)))))))
	      ("f" . ,(attr 'source))
	      ("i" . (lambda (context) (profile-image)))
	      ("j" . ,(attr 'user-id))
	      ("L" . ,(let ((location (or (attr 'user-location) "")))
			(if (not (string= "" location))
			    (concat " [" location "]")
			  "")))
	      ("l" . ,(attr 'user-location))
	      ("p" . ,(if (string= "true" (attr 'user-protected))
			  "[x]"
			""))
	      ("r" .
	       ,(let ((reply-id (or (attr 'in-reply-to-status-id) ""))
		      (reply-name (or (attr 'in-reply-to-screen-name) "")))
		  (if (or (string= "" reply-id) (string= "" reply-name))
		      ""
		    (let ((in-reply-to-string
			   (concat "to @" reply-name))
			  (url
			   (twitter1-get-status-url reply-name reply-id)))
		      (concat " "
			      (make-string-with-url-property
			       in-reply-to-string url))))))
	      ("R" .
	       ,(let ((retweeted-by (attr 'original-user-screen-name)))
		  (if retweeted-by
		      (concat "RT @" retweeted-by)
		    "")))

	      ("S" . ,(attr 'user-name))
	      ("s" . ,(attr 'user-screen-name))
	      ("T" . ,(attr 'text))
	      ("t\\([^\n]*\\)" .
	       ,(lambda (context)
		  (let* ((str (cdr (assq 'processed-string context)))
			 (prefix (if (string-match "\\([^\n]*\\)\\'" str)
				     (match-string 1 str)
				       ""))
			 (following-str (cdr (assq 'following-string context)))
			 (from (cdr (assq 'from context)))
			 (match-data (cdr (assq 'match-data context)))
			 (replace-prefix (cdr (assq 'prefix context)))
			 (table (cdr (assq 'replacement-table context))))
		    (store-match-data match-data)
		    (let* ((postfix (twitter1-format-string
				     (match-string 1 following-str)
				     replace-prefix table))
			   (text (concat prefix (attr 'text) postfix)))
		      (with-temp-buffer
			(insert text)
			(fill-region-as-paragraph (point-min) (point-max))
			(buffer-substring (1+ (length prefix)) (point-max)))))
		  ))
	      ("u" . ,(attr 'user-url))
	      ))
	   (formatted-status
	    (twitter1-format-string format-str "%" replace-table)))
      (add-text-properties
       0 (length formatted-status)
       `(username ,(attr 'user-screen-name) id ,(attr 'id) text ,(attr 'text))
       formatted-status)
      formatted-status)))

(defun twitter1-timer-action (func)
  (let ((buf (get-buffer twitter1-buffer)))
    (if (null buf)
	(twitter1-stop)
      (funcall func)
      )))

(defun twitter1-show-minibuffer-length (&optional beg end len)
  "Show the number of charactors in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
	(deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
	   (status-len (- (buffer-size) (minibuffer-prompt-width)))
	   (sign-len (length (twitter1-sign-string)))
	   (mes (if (< 0 sign-len)
		    (format "%d=%d+%d"
			    (+ status-len sign-len) status-len sign-len)
		  (format "%d" status-len))))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun twitter1-setup-minibuffer ()
  (add-hook 'post-command-hook 'twitter1-show-minibuffer-length t t))

(defun twitter1-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twitter1-show-minibuffer-length t))

(defun twitter1-status-not-blank-p (status)
  (with-temp-buffer
    (insert status)
    (goto-char (point-min))
    ;; skip user name
    (re-search-forward "@[-_a-z0-9]+\\([\n\r \t]+@[-_a-z0-9]+\\)*" nil t)
    (re-search-forward "[^\n\r \t]+" nil t)))

(defun twitter1-update-status-from-minibuffer (&optional init-str reply-to-id)
  (when (and (null init-str)
	     twitter1-current-hashtag)
    (setq init-str (format " #%s " twitter1-current-hashtag)))
  (let ((status init-str)
	(sign-str (twitter1-sign-string))
	(not-posted-p t)
	(prompt "status: ")
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'twitter1-tinyurl-replace-at-point)
    (when twitter1-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twitter1-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twitter1-finish-minibuffer t))
    (unwind-protect
	(while not-posted-p
	  (setq status (read-from-minibuffer prompt status map nil 'twitter1-tweet-history nil t))
	  (let ((status-with-sign (concat status sign-str)))
	    (if (< 140 (length status-with-sign))
		(setq prompt "status (too long): ")
	      (progn
		(setq prompt "status: ")
		(when (twitter1-status-not-blank-p status)
		  (let ((parameters `(("status" . ,status-with-sign)
				      ("source" . "twmode")
				      ,@(if reply-to-id
					    `(("in_reply_to_status_id"
					       . ,reply-to-id))))))
		    (twitter1-http-post "twitter.com" "statuses/update" parameters)
		    (setq not-posted-p nil)))
		))))
      ;; unwindforms
      (when (memq 'twitter1-setup-minibuffer minibuffer-setup-hook)
	(remove-hook 'minibuffer-setup-hook 'twitter1-setup-minibuffer))
      (when (memq 'twitter1-finish-minibuffer minibuffer-exit-hook)
	(remove-hook 'minibuffer-exit-hook 'twitter1-finish-minibuffer))
      )))

(defun twitter1-get-list-index (username)
  (twitter1-http-get "api.twitter.com"
		       (concat "1/" username "/lists")
		       t nil
		       'twitter1-http-get-list-index-sentinel))

(defun twitter1-get-list-index-sync (username)
  (setq twitter1-list-index-retrieved nil)
  (twitter1-get-list-index username)
  (while (not twitter1-list-index-retrieved)
    (sit-for 0.1))
  (cond
   ((stringp twitter1-list-index-retrieved)
    (if (string= "" twitter1-list-index-retrieved)
	(message (concat username " has no list"))
      (message twitter1-list-index-retrieved))
    nil)
   ((listp twitter1-list-index-retrieved)
    twitter1-list-index-retrieved)))

(defun twitter1-manage-friendships (method username)
  (twitter1-http-post "twitter.com"
			(concat "friendships/" method)
			`(("screen_name" . ,username)
			  ("source" . "twmode"))))

(defun twitter1-manage-favorites (method id)
  (twitter1-http-post "twitter.com"
			(concat "favorites/" method "/" id)
			`(("source" . "twmode"))))

(defun twitter1-get-twits (host method &optional noninteractive id)
  (let ((buf (get-buffer twitter1-buffer)))
    (if (not buf)
	(twitter1-stop)
      (let* ((default-count 20)
	     (count twitter1-number-of-tweets-on-retrieval)
	     (count (cond
		     ((integerp count) count)
		     ((string-match "^[0-9]+$" count)
		      (string-to-number count 10))
		     (t default-count)))
	     (count (min (max 1 count)
			 twitter1-max-number-of-tweets-on-retrieval))
	     (regexp-list-method "^1/[^/]*/lists/[^/]*/statuses$")
	     (parameters
	      (list (cons (if (string-match regexp-list-method method)
			      "per_page"
			    "count")
			  (number-to-string count)))))
	(if id
	    (add-to-list 'parameters `("max_id" . ,id))
	  (when twitter1-timeline-last-update
	    (let* ((system-time-locale "C")
		   (since
		    (twitter1-global-strftime
		     "%a, %d %b %Y %H:%M:%S GMT"
		     twitter1-timeline-last-update)))
	      (add-to-list 'parameters `("since" . ,since)))))
	(twitter1-http-get host method
			     noninteractive parameters))))

  (if (and twitter1-icon-mode window-system
	   twitter1-image-stack)
      (mapc 'twitter1-retrieve-image twitter1-image-stack)
    ))

(defun twitter1-get-and-render-timeline (spec &optional noninteractive id)
  (let* ((original-spec spec)
	 (spec-string (if (stringp spec)
			  spec
			(twitter1-timeline-spec-to-string spec)))
	 (spec ;; normalized spec.
	  (twitter1-string-to-timeline-spec spec-string)))
    (when (null spec)
      (error "\"%s\" is invalid as a timeline spec"
	     (or spec-string original-spec)))
    (setq twitter1-last-requested-timeline-spec-string spec-string)
    (unless
	(and twitter1-last-retrieved-timeline-spec-string
	     (twitter1-equal-string-as-timeline
	      spec-string twitter1-last-retrieved-timeline-spec-string))
      (setq twitter1-timeline-last-update nil
	    twitter1-timeline-data nil))
    (if (twitter1-timeline-spec-primary-p spec)
	(let ((pair (twitter1-timeline-spec-to-host-method spec)))
	  (when pair
	    (let ((host (car pair))
		  (method (cadr pair)))
	      (twitter1-get-twits host method noninteractive id))))
      (let ((type (car spec)))
	(error "%s has not been supported yet" type)))))

(defun twitter1-retrieve-image (image-url)
  (let ((image-data (gethash `(,image-url . ,twitter1-convert-fix-size)
			     twitter1-image-data-table)))
    (when (not image-data)
      (let ((image-type nil)
	    (image-spec nil)
	    (converted-image-size
	     `(,twitter1-convert-fix-size . ,twitter1-convert-fix-size)))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		(require-final-newline nil))
	    (url-insert-file-contents image-url)
	    (setq image-type (twitter1-image-type image-url
						    (current-buffer)))
	    (setq image-spec `(image :type ,image-type
				     :data ,(buffer-string)))
	    (when (and twitter1-convert-fix-size twitter1-use-convert
		       (not
			(and (image-type-available-p image-type)
			     (equal (image-size image-spec t)
				    converted-image-size))))
	      (call-process-region 
	       (point-min) (point-max)
	       twitter1-convert-program
	       t t nil
	       (if image-type (format "%s:-" image-type) "-")
	       "-resize"
	       (format "%dx%d" twitter1-convert-fix-size
		       twitter1-convert-fix-size)
	       "xpm:-")
	      (setq image-type 'xpm))
	    (setq image-data `(,image-type . ,(buffer-string))))
	  (puthash `(,image-url . ,twitter1-convert-fix-size)
		   image-data
		   twitter1-image-data-table))))
    image-data))

(defun twitter1-tinyurl-get (longurl)
  "Tinyfy LONGURL"
  (let ((api (cdr (assoc twitter1-tinyurl-service
			 twitter1-tinyurl-services-map))))
    (unless api
      (error "Invaild `twitter1-tinyurl-service'. try one of %s"
	     (concat (mapconcat (lambda (x)
				  (symbol-name (car x)))
				twitter1-tinyurl-services-map ", "))))
    (if longurl
	(let ((buffer (url-retrieve-synchronously (concat api longurl))))
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (prog1
		(if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
		    (match-string-no-properties 1)
		  (error "TinyURL failed: %s" longurl))
	      (kill-buffer buffer))))
      nil)))

;;;
;;; Commands
;;;

(defun twitter1-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twitter1-current-timeline-noninteractive))
  (if twitter1-timer
      nil
    (setq twitter1-timer
	  (run-at-time "0 sec"
		       twitter1-timer-interval
		       #'twitter1-timer-action action))))

(defun twitter1-stop ()
  (interactive)
  (when twitter1-timer
    (cancel-timer twitter1-timer)
    (setq twitter1-timer nil)))

(defun twitter1-scroll-mode (&optional arg)
  (interactive)
  (setq twitter1-scroll-mode
	(if (null arg)
	    (not twitter1-scroll-mode)
	  (> (prefix-numeric-value arg) 0)))
  (twitter1-update-mode-line))

(defun twitter1-jojo-mode (&optional arg)
  (interactive)
  (setq twitter1-jojo-mode
	(if (null arg)
	    (not twitter1-jojo-mode)
	  (> (prefix-numeric-value arg) 0)))
  (twitter1-update-mode-line))

(defun twitter1-friends-timeline ()
  (interactive)
  (twitter1-get-and-render-timeline '(friends)))

(defun twitter1-replies-timeline ()
  (interactive)
  (twitter1-get-and-render-timeline '(replies)))

(defun twitter1-public-timeline ()
  (interactive)
  (twitter1-get-and-render-timeline '(public)))

(defun twitter1-user-timeline ()
  (interactive)
  (twitter1-get-and-render-timeline `(user ,(twitter1-get-username))))

(defun twitter1-current-timeline-noninteractive ()
  (twitter1-current-timeline t))

(defun twitter1-current-timeline (&optional noninteractive)
  (interactive)
  (let ((spec (or twitter1-last-retrieved-timeline-spec-string
		  twitter1-initial-timeline-spec-string)))
    (twitter1-get-and-render-timeline spec noninteractive)))

(defun twitter1-update-status-interactive ()
  (interactive)
  (twitter1-update-status-from-minibuffer))

(defun twitter1-update-lambda ()
  (interactive)
  (when (and (string-equal "Japanese" current-language-environment)
	     (or (> emacs-major-version 21)
		 (eq 'utf-8 (terminal-coding-system))))
    (twitter1-http-post
     "twitter.com"
     "statuses/update"
     `(("status" . ,(mapconcat
		     'char-to-string
		     (mapcar 'twitter1-ucs-to-char
			     '(955 12363 12431 12356 12356 12424 955)) ""))
       ("source" . "twmode")))))

(defun twitter1-update-jojo (usr msg)
  (when (and (string-equal "Japanese" current-language-environment)
	     (or (> emacs-major-version 21)
		 (eq 'utf-8 (terminal-coding-system))))
    (if (string-match
	 (mapconcat
	  'char-to-string
	  (mapcar 'twitter1-ucs-to-char
		  '(27425 12395 92 40 12362 21069 92 124 36020 27096
			  92 41 12399 12300 92 40 91 94 12301 93 43 92 
			  41 12301 12392 35328 12358)) "")
	 msg)
	(twitter1-http-post
	 "twitter.com"
	 "statuses/update"
	 `(("status" . ,(concat
			 "@" usr " "
			 (match-string-no-properties 2 msg)
			 (string-as-multibyte
			  (if (>= emacs-major-version 23)
			      "\343\200\200\343\201\257\343\201\243!?"
			    "\222\241\241\222\244\317\222\244\303!?"))))
	   ("source" . "twmode"))))))

(defun twitter1-set-current-hashtag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (twitter1-completing-read "hashtag (blank to clear): #"
					  twitter1-hashtag-history
					  nil nil
					  twitter1-current-hashtag
					  'twitter1-hashtag-history))
    (message
     (if (eq 0 (length tag))
	 (progn (setq twitter1-current-hashtag nil)
		"Current hashtag is not set.")
       (progn
	 (setq twitter1-current-hashtag tag)
	 (format "Current hashtag is #%s" twitter1-current-hashtag))))))

(defun twitter1-erase-old-statuses ()
  (interactive)
  (setq twitter1-timeline-data nil)
  (if (not twitter1-last-retrieved-timeline-spec-string)
      (setq twitter1-last-retrieved-timeline-spec-string
	    twitter1-initial-timeline-spec-string)
    (let* ((spec-string twitter1-last-retrieved-timeline-spec-string)
	   (spec (twitter1-string-to-timeline-spec spec-string))
	   (pair (twitter1-timeline-spec-to-host-method spec))
	   (host (car pair))
	   (method (cadr pair)))
      (if (not twitter1-timeline-last-update)
	  (twitter1-http-get host method)
	(let* ((system-time-locale "C")
	       (since
		(twitter1-global-strftime
		 "%a, %d %b %Y %H:%M:%S GMT"
		 twitter1-timeline-last-update)))
	  (twitter1-http-get host method nil `(("since" . ,since))))))))

(defun twitter1-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twitter1-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(uri (get-text-property (point) 'uri))
	(uri-in-text (get-text-property (point) 'uri-in-text))
	(screen-name-in-text
	 (get-text-property (point) 'screen-name-in-text)))
    (cond (screen-name-in-text
	   (twitter1-update-status-from-minibuffer
	    (concat "@" screen-name-in-text " ") id))
	  (uri-in-text
	   (browse-url uri-in-text))
	  (username
	   (twitter1-update-status-from-minibuffer
	    (concat "@" username " ") id))
	  (uri
	   (browse-url uri)))))

(defun twitter1-tinyurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (twitter1-tinyurl-get (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))

(defun twitter1-retweet ()
  (interactive)
  (if twitter1-use-native-retweet
      (twitter1-native-retweet)
    (twitter1-organic-retweet)))

(defun twitter1-organic-retweet ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(text (get-text-property (point) 'text))
	(id (get-text-property (point) 'id))
	(retweet-time (current-time))
	(format-str (or twitter1-retweet-format
			"RT: %t (via @%s)")))
    (when username
      (let ((prefix "%")
	    (replace-table
	     `(("%" . "%")
	       ("s" . ,username)
	       ("t" . ,text)
	       ("#" . ,id)
	       ("C{\\([^}]*\\)}" .
		(lambda (context)
		  (let ((str (cdr (assq 'following-string context)))
			(match-data (cdr (assq 'match-data context))))
		    (store-match-data match-data)
		    (format-time-string (match-string 1 str) ',retweet-time))))
	       ))
	    )
	(twitter1-update-status-from-minibuffer
	 (twitter1-format-string format-str prefix replace-table))
	))))

(defun twitter1-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twitter1-follow (&optional remove)
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(method (if remove "destroy" "create"))
	(mes (if remove "unfollowing" "following")))
    (unless username
      (setq username (twitter1-read-username-with-completion
		      "who: " "" 'twitter1-user-history)))
    (if (> (length username) 0)
	(when (y-or-n-p (format "%s %s? " mes username))
	  (twitter1-manage-friendships method username))
      (message "No user selected"))))

(defun twitter1-unfollow ()
  (interactive)
  (twitter1-follow t))

(defun twitter1-native-retweet ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (get-text-property (point) 'text))
	(len 25))
    (if id
	(let ((mes (format "Retweet \"%s\"? "
			   (if (> (length text) len)
			       (concat (substring text 0 len) "...")
			     text))))
	  (when (y-or-n-p mes)
	    (twitter1-http-post "api.twitter.com"
			(concat "1/statuses/retweet/" id)
			`(("source" . "twmode")))))
      (message "No status selected"))))

(defun twitter1-favorite (&optional remove)
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (get-text-property (point) 'text))
	(len 25) ;; XXX
	(method (if remove "destroy" "create")))
    (if id
	(let ((mes (format "%s \"%s\"? "
			   (if remove "unfavorite" "favorite")
			   (if (> (length text) len)
			       (concat (substring text 0 len) "...")
			     text))))
	  (when (y-or-n-p mes)
	    (twitter1-manage-favorites method id)))
      (message "No status selected"))))

(defun twitter1-unfavorite ()
  (interactive)
  (twitter1-favorite t))

(defun twitter1-visit-timeline (&optional timeline-spec initial)
  (interactive)
  (let ((timeline-spec
	 (or timeline-spec
	     (twitter1-read-timeline-spec-with-completion
	      "timeline: " initial t))))
    (when timeline-spec
      (twitter1-get-and-render-timeline timeline-spec))))

(defun twitter1-other-user-timeline ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (spec (cond (screen-name-in-text `(user ,screen-name-in-text))
		     (username `(user ,username))
		     (t nil))))
    (if spec
	(twitter1-get-and-render-timeline spec)
      (message "No user selected"))))

(defun twitter1-other-user-timeline-interactive ()
  (interactive)
  (let ((username
	 (twitter1-read-username-with-completion
	  "user: " nil
	  'twitter1-user-history)))
    (if (> (length username) 0)
	(twitter1-get-and-render-timeline `(user ,username))
      (message "No user selected"))))

(defun twitter1-other-user-list-interactive ()
  (interactive)
  (let ((username (twitter1-read-username-with-completion
		   "whose list: "
		   (get-text-property (point) 'username)
		   'twitter1-user-history)))
    (if (string= "" username)
	(message "No user selected")
      (let* ((list-name (twitter1-read-list-name username))
	     (spec `(list ,username ,list-name)))
	(when list-name
	  (twitter1-get-and-render-timeline spec))))))

(defun twitter1-direct-message ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twitter1-update-status-from-minibuffer (concat "d " username " ")))))

(defun twitter1-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twitter1-update-status-from-minibuffer (concat "@" username " ")))))

(defun twitter1-make-list-from-assoc (key data)
  (mapcar (lambda (status)
	    (cdr (assoc key status)))
	  data))

(defun twitter1-read-username-with-completion (prompt init-user &optional history)
  (let ((collection
	 (append (twitter1-make-list-from-assoc
		  'user-screen-name twitter1-timeline-data)
		 twitter1-user-history)))
    (twitter1-completing-read prompt collection nil nil init-user history)))

(defun twitter1-read-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twitter1-get-list-index-sync username)))
	 (prompt (concat username "'s list: "))
	 (listname
	  (if list-index
	      (twitter1-completing-read prompt list-index nil t nil)
	    nil)))
    (if (string= "" listname)
	nil
      listname)))

(defun twitter1-read-timeline-spec-with-completion (prompt initial &optional as-string)
  (let* ((dummy-hist (append twitter1-timeline-history
			     (twitter1-make-list-from-assoc
			      'user-screen-name twitter1-timeline-data)))
	 (spec-string (twitter1-completing-read prompt dummy-hist
						  nil nil initial 'dummy-hist))
	 (spec-string
	  (if (string-match "^\\([^/]+\\)/$" spec-string)
	      (let* ((username (match-string 1 spec-string))
		     (list-index (twitter1-get-list-index-sync username))
		     (listname
		      (if list-index
			  (twitter1-read-list-name username list-index)
			nil)))
		(if listname
		    (concat username "/" listname)
		  nil))
	    spec-string))
	 (spec (twitter1-string-to-timeline-spec spec-string)))
    (cond
     (spec (if as-string
	       spec-string
	     spec))
     ((string= "" spec-string)
      (message "No timeline specs are specified.")
      nil)
     (t
      (message "\"%s\" is invalid as a timeline spec." spec-string)
      nil))))

(defun twitter1-get-username ()
  (or twitter1-username-active
      (setq twitter1-username-active (read-string "your twitter username: "))))

(defun twitter1-get-password ()
  (or twitter1-password-active
      (setq twitter1-password-active (read-passwd "your twitter password: "))))

(defun twitter1-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twitter1-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (let ((id (get-text-property (point) 'id)))
        (if id
	    (twitter1-get-and-render-timeline
	     twitter1-last-retrieved-timeline-spec-string
	     nil id))))))

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
  (let* ((current-pos (point))
         (prev-pos (twitter1-get-previous-username-face-pos current-pos)))
    (if (and prev-pos (not (eq current-pos prev-pos)))
        (goto-char prev-pos)
      (message "Start of status."))))

(defun twitter1-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter1-username-face)))
	(setq pos (previous-single-property-change pos 'face))
	(when (eq pos nil)
	  (let ((head-prop (get-text-property (point-min) 'face)))
	    (if (and
		 (not (eq prop twitter1-username-face))
		 (eq head-prop twitter1-username-face))
		(setq pos (point-min))
	      (throw 'not-found nil)
	      )))
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
        (prev-pos (point))
	(pos (twitter1-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (eq pos prev-pos))
		(not (equal (twitter1-get-username-at-pos pos) user-name)))
      (setq prev-pos pos)
      (setq pos (twitter1-get-previous-username-face-pos pos)))
    (if (and pos
             (not (eq pos prev-pos))
             (equal (twitter1-get-username-at-pos pos) user-name))
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twitter1-goto-next-thing (&optional backword)
  "Go to next interesting thing. ex) username, URI, ... "
  (interactive)
  (let* ((propety-change-f (if backword
			       'previous-single-property-change
			     'next-single-property-change))
	 (pos (funcall propety-change-f (point) 'face)))
    (while (and pos
		(not 
		 (let* ((current-face (get-text-property pos 'face))
			(face-pred
			 (lambda (face)
			   (cond
			    ((listp current-face) (memq face current-face))
			    ((symbolp current-face) (eq face current-face))
			    (t nil)))))
		   (member-if face-pred
			      '(twitter1-username-face
				twitter1-uri-face)))))
      (setq pos (funcall propety-change-f pos 'face)))
    (when pos
      (goto-char pos))))

(defun twitter1-goto-previous-thing (&optional backword)
  "Go to previous interesting thing. ex) username, URI, ... "
  (interactive)
  (twitter1-goto-next-thing (not backword)))

(defun twitter1-get-username-at-pos (pos)
  (or (get-text-property pos 'username)
      (get-text-property (max (point-min) (1- pos)) 'username)
      (let* ((border (or (previous-single-property-change pos 'username)
                         (point-min)))
             (pos (max (point-min) (1- border))))
        (get-text-property pos 'username))))

(defun twitter1-mode ()
  "Major mode for Twitter
\\{twitter1-mode-map}"
  (interactive)
  (switch-to-buffer (twitter1-buffer))
  (kill-all-local-variables)
  (twitter1-mode-init-variables)
  (use-local-map twitter1-mode-map)
  (setq major-mode 'twitter1-mode)
  (twitter1-update-mode-line)
  (set-syntax-table twitter1-mode-syntax-table)
  (run-hooks 'twitter1-mode-hook)
  (font-lock-mode -1)
  (twitter1-stop)
  (twitter1-start))

(defun twitter1-suspend ()
  "Suspend twitter1-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun twit ()
  "Start twitter1-mode."
  (interactive)
  (twitter1-mode))

(provide 'twitter1-mode)
;;; twitter1.el ends here
