;;; twitter6-mode.el --- Major mode for Twitter

;; Copyright (C) 2007, 2009, 2010 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Forked by id774 <idnanashi@gmail.com> to following changes:
;; Cutback codes and functions.
;; Divide namespace of twittering-mode into 6 accounts.
;; Change status format, Add keybind for window move and quick operation.
;; URL: http://github.com/id774/scripts

;; Original Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;; Created: Sep 4, 2007
;; Original Version: 0.4
;; Keywords: twitter web
;; URL: http://github.com/hayamiz/twittering-mode/

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

;; twitter6-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)

(defconst twitter6-mode-version "0.8")

(defun twitter6-mode-version ()
  "Display a message for twitter6-mode version."
  (interactive)
  (let ((version-string
         (format "twitter6-mode-v%s" twitter6-mode-version)))
    (if (interactive-p)
        (message "%s" version-string)
      version-string)))

(setq global-hl-line-mode nil)

(defconst twitter6-http-status-line-regexp
  "HTTP/1\.[01] \\(\\([0-9][0-9][0-9]\\) [^\r\n]+\\)\r?\n"
  "Regular expression used in \"sentinel\" functions to pick up
status-code and reason-phrase from the response.")

(defvar twitter6-mode-map (make-sparse-keymap))

(defvar twitter6-timer nil "Timer object for timeline refreshing will be
stored here. DO NOT SET VALUE MANUALLY.")

(defvar twitter6-idle-time 18)

(defvar twitter6-timer-interval 94)

(defvar twitter6-username nil)

(defvar twitter6-password nil)

(defvar twitter6-last-timeline-retrieved nil)

(defvar twitter6-last-timeline-interactive nil)

(defvar twitter6-new-tweets-count 0
  "Number of new tweets when `twitter6-new-tweets-hook' is run")

(defvar twitter6-new-tweets-hook nil
  "Hook run when new twits are received.

You can read `twitter6-new-tweets-count' to get the number of new
tweets received when this hook is run.")

(defvar twitter6-scroll-mode nil)
(make-variable-buffer-local 'twitter6-scroll-mode)

(defvar twitter6-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twitter6-status-format nil)
(setq twitter6-status-format "%i %s: %t %p [%C]")
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

(defvar twitter6-buffer "*twitter6*")
(defun twitter6-buffer ()
  (twitter6-get-or-generate-buffer twitter6-buffer))

(defvar twitter6-http-buffer "*twitter6-http-buffer*")
(defun twitter6-http-buffer ()
  (twitter6-get-or-generate-buffer twitter6-http-buffer))

(defvar twitter6-timeline-data nil)
(defvar twitter6-timeline-last-update nil)

(defvar twitter6-username-face 'twitter6-username-face)
(defvar twitter6-uri-face 'twitter6-uri-face)

(defun twitter6-get-or-generate-buffer (buffer)
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

(defvar twitter6-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)")

;;; Proxy
(defvar twitter6-proxy-use global-proxy-use)
(defvar twitter6-proxy-server global-proxy-server)
(defvar twitter6-proxy-port global-proxy-port)
(defvar twitter6-proxy-user global-proxy-user)
(defvar twitter6-proxy-password global-proxy-password)

(defun twitter6-toggle-proxy () ""
  (interactive)
  (setq twitter6-proxy-use
        (not twitter6-proxy-use))
  (message "%s %s"
           "Use Proxy:"
           (if twitter6-proxy-use
               "on" "off")))

(defun twitter6-user-agent-default-function ()
  "twitter6 mode default User-Agent function."
  (concat "Emacs/"
          (int-to-string emacs-major-version) "." (int-to-string
                                                   emacs-minor-version)
          " "
          "twitter6-mode/"
          twitter6-mode-version))

(defvar twitter6-sign-simple-string nil)

(defun twitter6-sign-string-default-function ()
  "Tweet append sign string:simple "
  (if twitter6-sign-simple-string
      (concat " [" twitter6-sign-simple-string "]")
    ""))

(defvar twitter6-user-agent-function 'twitter6-user-agent-default-function)
(defvar twitter6-sign-string-function 'twitter6-sign-string-default-function)

(defun twitter6-user-agent ()
  "Return User-Agent header string."
  (funcall twitter6-user-agent-function))

(defun twitter6-sign-string ()
  "Return Tweet sign string."
  (funcall twitter6-sign-string-function))

;;; to show image files

(defvar twitter6-wget-buffer "*twitter6-wget-buffer*")
(defun twitter6-wget-buffer ()
  (twitter6-get-or-generate-buffer twitter6-wget-buffer))

(defvar twitter6-tmp-dir
  (expand-file-name (concat "twmode-images-" (user-login-name))
                    temporary-file-directory))

(defvar twitter6-icon-mode nil "You MUST NOT CHANGE this variable
directory. You should change through function'twitter6-icon-mode'")

(make-variable-buffer-local 'twitter6-icon-mode)
(defun twitter6-icon-mode (&optional arg)
  (interactive)
  (setq twitter6-icon-mode
        (if twitter6-icon-mode
            (if (null arg)
                nil
              (> (prefix-numeric-value arg) 0))
          (when (or (null arg)
                    (and arg (> (prefix-numeric-value arg) 0)))
            (when (file-writable-p twitter6-tmp-dir)
              (progn
                (if (not (file-directory-p twitter6-tmp-dir))
                    (make-directory twitter6-tmp-dir))
                t)))))
  (twitter6-render-timeline))

(defun twitter6-scroll-mode (&optional arg)
  (interactive)
  (setq twitter6-scroll-mode
        (if (null arg)
            (not twitter6-scroll-mode)
          (> (prefix-numeric-value arg) 0))))

(defvar twitter6-image-stack nil)

(defun twitter6-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun twitter6-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
                      (apply 'encode-time (parse-time-string string))
                      uni))
(defun twitter6-local-strftime (fmt string)
  (twitter6-setftime fmt string nil))
(defun twitter6-global-strftime (fmt string)
  (twitter6-setftime fmt string t))

(defvar twitter6-debug-mode nil)
(defvar twitter6-debug-buffer "*debug*")
(defun twitter6-debug-buffer ()
  (twitter6-get-or-generate-buffer twitter6-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twitter6-debug-mode
           (with-current-buffer (twitter6-debug-buffer)
             (insert (prin1-to-string ,obsym))
             (newline)
             ,obsym)
         ,obsym))))

(defun twitter6-debug-mode ()
  (interactive)
  (setq twitter6-debug-mode
        (not twitter6-debug-mode))
  (message (if twitter6-debug-mode "debug mode:on" "debug mode:off")))

(if twitter6-mode-map
    (let ((km twitter6-mode-map))
      (define-key km "\C-c\C-f" 'twitter6-friends-timeline)
      (define-key km "e" 'twitter6-friends-timeline)
      (define-key km "\C-c\C-r" 'twitter6-replies-timeline)
      (define-key km "r" 'twitter6-replies-timeline)
      (define-key km "\C-c\C-g" 'twitter6-public-timeline)
      (define-key km "\C-c\C-u" 'twitter6-user-timeline)
      (define-key km "u" 'twitter6-user-timeline)
      (define-key km "\C-c\C-s" 'twitter6-update-status-interactive)
      (define-key km "w" 'twitter6-update-status-interactive)
      (define-key km "\C-c\C-d" 'twitter6-erase-old-statuses)
      (define-key km "\C-m" 'twitter6-enter)
      (define-key km [mouse-1] 'twitter6-click)
      (define-key km "\C-c\C-v" 'twitter6-view-user-page)
      (define-key km "g" 'twitter6-current-timeline)
      (define-key km "c" 'twitter6-current-timeline-interactive)
      (define-key km "i" 'twitter6-other-user-timeline)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twitter6-goto-next-status)
      (define-key km "k" 'twitter6-goto-previous-status)
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
      ;; (define-key km "n" 'twitter6-goto-next-status-of-user)
      ;; (define-key km "p" 'twitter6-goto-previous-status-of-user)
      (define-key km "n" 'windmove-down)
      (define-key km "p" 'windmove-up)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "\C-c\ i" 'twitter6-icon-mode)
      (define-key km "s" 'twitter6-scroll-mode)
      (define-key km "a" 'twitter6-favorite)
      (define-key km "q" 'twitter6-unfavorite)
      (define-key km "t" 'twitter6-toggle-proxy)
      (define-key km "\C-c\C-p" 'twitter6-toggle-proxy)
      nil))

(defvar twitter6-mode-syntax-table nil "")

(if twitter6-mode-syntax-table
    ()
  (setq twitter6-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twitter6-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twitter6-mode-syntax-table)
  )

(defun twitter6-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface twitter6-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twitter6-username-face)
  (set-face-attribute 'twitter6-username-face nil :underline t)
  (defface twitter6-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twitter6-uri-face nil :underline t)
  (add-to-list 'minor-mode-alist '(twitter6-icon-mode " tw-icon"))
  (add-to-list 'minor-mode-alist '(twitter6-scroll-mode " tw-scroll"))
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
(defmacro twitter6-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar twitter6-mode-string "twitter6 mode")

(defvar twitter6-mode-hook nil
  "twitter6-mode hook.")

(defun twitter6-mode ()
  "Major mode for Twitter
\\{twitter6-mode-map}"
  (interactive)
  (switch-to-buffer (twitter6-buffer))
  (kill-all-local-variables)
  (twitter6-mode-init-variables)
  (use-local-map twitter6-mode-map)
  (setq major-mode 'twitter6-mode)
  (setq mode-name twitter6-mode-string)
  (set-syntax-table twitter6-mode-syntax-table)
  (run-hooks 'twitter6-mode-hook)
  (font-lock-mode -1)
  (twitter6-start))

;;;
;;; Basic HTTP functions
;;;

(defun twitter6-start-http-session (method headers host port path parameters &optional noninteractive sentinel)
  "
METHOD    : http method
HEADERS   : http request heades in assoc list
HOST      : remote host name
PORT      : destination port number. nil means default port (http: 80, https: 443)
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
      (setq headers (cons `("User-Agent" . ,(twitter6-user-agent))
                          headers)))

  (twitter6-start-http-non-ssl-session
   method headers host port path parameters
   noninteractive sentinel)))

(defun twitter6-http-get (method-class method &optional parameters sentinel)
  (if (null sentinel) (setq sentinel 'twitter6-http-get-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter6-http-buffer))
    (erase-buffer))

  (let (proc server port
             (proxy-user twitter6-proxy-user)
             (proxy-password twitter6-proxy-password))
    (condition-case nil
        (progn
          (if (and twitter6-proxy-use twitter6-proxy-server)
              (setq server twitter6-proxy-server
                    port (if (integerp twitter6-proxy-port)
                             (int-to-string twitter6-proxy-port)
                           twitter6-proxy-port))
            (setq server "twitter.com"
                  port "80"))
          (setq proc
                (open-network-stream
                 "network-connection-process" (twitter6-http-buffer)
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
                                                (twitter6-percent-encode (car
                                                                            param-pair))
                                                (twitter6-percent-encode (cdr
                                                                            param-pair))))
                                      parameters
                                      "&")))
                           " HTTP/1.1" nl
                           "Host: twitter.com" nl
                           "User-Agent: " (twitter6-user-agent) nl
                           "Authorization: Basic "
                           (base64-encode-string
                            (concat twitter6-username ":"
                                    (twitter6-get-password)))
                           nl
                           "Accept: text/xml"
                           ",application/xml"
                           ",application/xhtml+xml"
                           ",application/html;q=0.9"
                           ",text/plain;q=0.8"
                           ",image/png,*/*;q=0.5" nl
                           "Accept-Charset: utf-8;q=0.7,*;q=0.7" nl
                           (when twitter6-proxy-use
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

(defun twitter6-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (twitter6-get-response-header))
        (body (twitter6-get-response-body))
        (status nil)
        )
    (if (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header)
        (progn
          (setq status (match-string-no-properties 1 header))
          (case-string
           status
           (("200 OK")
            (setq twitter6-new-tweets-count
                  (count t (mapcar
                            #'twitter6-cache-status-datum
                            (reverse (twitter6-xmltree-to-status
                                      body)))))
            (if (and (> twitter6-new-tweets-count 0)
                     (not twitter6-last-timeline-interactive))
                (run-hooks 'twitter6-new-tweets-hook))
            (setq twitter6-last-timeline-interactive t)
            (twitter6-render-timeline))
           (t (message status))))
      (message "Failure: Bad http response.")))
  )

(defun twitter6-render-timeline ()
  (with-current-buffer (twitter6-buffer)
    (let ((point (point))
          (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
              (insert (twitter6-format-status
                       status twitter6-status-format))
              (fill-region-as-paragraph
               (save-excursion (beginning-of-line) (point)) (point))
              (insert "\n"))
            twitter6-timeline-data)
      (if (and twitter6-image-stack window-system)
          (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twitter6-scroll-mode (- (point-max) end) 0))))
    ))

(defun twitter6-format-status (status format-str)
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
                  (if (file-exists-p (concat twitter6-tmp-dir
                                             "/" filename))
                      t
                    (add-to-list 'twitter6-image-stack profile-image-url))

                  (when (and icon-string twitter6-icon-mode)
                    (set-text-properties
                     1 2 `(display
                           (image :type ,(twitter6-image-type filename)
                                  :file ,(concat twitter6-tmp-dir
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
          ((?r)                                ; %r - in_reply_to_status_id
           (let ((reply-id (attr 'in-reply-to-status-id))
                 (reply-name (attr 'in-reply-to-screen-name)))
             (unless (or (null reply-id) (string= "" reply-id)
                         (null reply-name) (string= "" reply-name))
               (let ((in-reply-to-string (format "in reply to %s" reply-name))
                     (url (twitter6-get-status-url reply-name reply-id)))
                 (add-text-properties
                  0 (length in-reply-to-string)
                  `(mouse-face highlight
                               face twitter6-uri-face
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
           (list-push (twitter6-local-strftime
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
               (setq url (twitter6-get-status-url (attr 'user-screen-name)
                                                    (attr 'id)))
               ;; make status url clickable
               (add-text-properties
                0 (length time-string)
                `(mouse-face highlight
                             face twitter6-uri-face
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

(defun twitter6-http-post
  (method-class method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com

METHOD-CLASS must be one of Twitter API method classes
 (statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twitter6-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter6-http-buffer))
    (erase-buffer))

  (let (proc server port
             (proxy-user twitter6-proxy-user)
             (proxy-password twitter6-proxy-password))
    (progn
      (if (and twitter6-proxy-use twitter6-proxy-server)
          (setq server twitter6-proxy-server
                port (if (integerp twitter6-proxy-port)
                         (int-to-string twitter6-proxy-port)
                       twitter6-proxy-port))
        (setq server "twitter.com"
              port "80"))
      (setq proc
            (open-network-stream
             "network-connection-process" (twitter6-http-buffer)
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
                                             (twitter6-percent-encode (car param-pair))
                                             (twitter6-percent-encode (cdr param-pair))))
                                   parameters
                                   "&")))
                        " HTTP/1.1" nl
                        "Host: twitter.com" nl
                        "User-Agent: " (twitter6-user-agent) nl
                        "Authorization: Basic "
                        (base64-encode-string
                         (concat twitter6-username ":" (twitter6-get-password)))
                        nl
                        "Content-Type: text/plain" nl
                        "Content-Length: 0" nl
                        (when twitter6-proxy-use
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

(defun twitter6-http-post-new (host method &optional parameters format sentinel)
  "Send HTTP POST request to twitter.com (or api.twitter.com)

HOST is hostname of remote side, twitter.com or api.twitter.com.
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (if (null format)
      (setq format "xml"))
  (if (null sentinel)
      (setq sentinel 'twitter6-http-post-default-sentinel-new))

  (twitter6-start-http-session
   "POST" (twitter6-http-application-headers "POST")
   host nil (concat "/" method "." format) parameters noninteractive sentinel))

(defun twitter6-http-default-sentinel-new (func noninteractive proc stat &optional suc-msg)
  (debug-printf "http-default-sentinel: proc=%s stat=%s" proc stat)
  (let ((temp-buffer (process-buffer proc)))
    (unwind-protect
        (let ((header (twitter6-get-response-header temp-buffer))
              (mes nil))
          (if (string-match twitter6-http-status-line-regexp header)
              (when (and func (fboundp func))
                (with-current-buffer temp-buffer
                  (setq mes (funcall func header proc noninteractive suc-msg))))
            (setq mes "Failure: Bad http response."))
          (when (and mes (twitter6-buffer-active-p))
            (message mes)))
      ;; unwindforms
      (twitter6-release-process proc)
      (when (and (not twitter6-debug-mode) (buffer-live-p temp-buffer))
        (kill-buffer temp-buffer))))
  )

(defun twitter6-start-http-non-ssl-session (method headers host port path parameters &optional noninteractive sentinel)
  (let ((request (twitter6-make-http-request
                  method headers host port path parameters))
        (temp-buffer (generate-new-buffer "*twmode-http-buffer*")))
    (flet ((request (key) (funcall request key)))
      (let* ((request-str
              (format "%s %s%s HTTP/1.1\r\n%s\r\n\r\n"
                      (request :method)
                      (request :uri)
                      (if parameters
                          (concat "?" (request :query-string))
                        "")
                      (request :headers-string)))
             (server (if twitter6-proxy-use
                         twitter6-proxy-server
                       (request :host)))
             (port (if twitter6-proxy-use
                       twitter6-proxy-port
                     (request :port)))
             (proc (open-network-stream
                    "network-connection-process" temp-buffer server port))
             )
        (lexical-let ((sentinel sentinel)
                      (noninteractive noninteractive))
          (set-process-sentinel
           proc
           (lambda (&rest args)
             (apply #'twitter6-http-default-sentinel
                    sentinel noninteractive args))))
        (debug-print request-str)
        (process-send-string proc request-str)
        proc)))
  )

(defun twitter6-make-http-request (method headers host port path parameters)
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
  (let* ((schema "http")
         (default-port 80)
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
                        (twitter6-percent-encode (car pair))
                        (twitter6-percent-encode (cdr pair))))
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

(defun twitter6-http-application-headers (&optional method headers)
  "Retuns an assoc list of HTTP headers for twitter6-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twitter6-user-agent)) headers)
    (push (cons "Authorization"
                (concat "Basic "
                        (base64-encode-string
                         (concat
                          twitter6-username
                          ":"
                          (twitter6-get-password)))))
          headers)
    (when (string= "GET" method)
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
    (when (string= "POST" method)
      (push (cons "Content-Length" "0") headers)
      (push (cons "Content-Type" "text/plain") headers))
                        (when twitter6-proxy-use
                          "Proxy-Connection: Keep-Alive" headers
                          (when (and twitter6-proxy-user twitter6-proxy-password)
                            (concat
                             "Proxy-Authorization: Basic "
                             (base64-encode-string
                              (concat twitter6-proxy-user ":"
                                      twitter6-proxy-password))
              headers)))
    headers
    ))

(defun twitter6-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
          (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twitter6-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%02x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter6-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?Z))
      (and (<= ?a ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twitter6-http-default-sentinel (func noninteractive proc stat &optional suc-msg)
  (debug-printf "http-default-sentinel: proc=%s stat=%s" proc stat)
  (let ((temp-buffer (process-buffer proc)))
    (unwind-protect
        (let ((header (twitter6-get-response-header temp-buffer))
              (mes nil))
          (if (string-match twitter6-http-status-line-regexp header)
              (when (and func (fboundp func))
                (with-current-buffer temp-buffer
                  (setq mes (funcall func header proc noninteractive suc-msg))))
            (setq mes "Failure: Bad http response."))
          (when (and mes (twitter6-buffer-active-p))
            (message mes)))
      ;; unwindforms
      (twitter6-release-process proc)
      (when (and (not twitter6-debug-mode) (buffer-live-p temp-buffer))
        (kill-buffer temp-buffer))))
  )

(defun twitter6-release-process (proc)
  (let ((spec (twitter6-get-timeline-spec-from-process proc)))
    (setq twitter6-process-info-alist
          (delete `(,proc ,spec) twitter6-process-info-alist))))

(defun twitter6-get-timeline-spec-from-process (proc)
  (let ((entry (assoc proc twitter6-process-info-alist)))
    (if entry
        (elt entry 1)
      nil)))

(defun debug-printf (fmt &rest args)
  (when twitter6-debug-mode
    (with-current-buffer (twitter6-debug-buffer)
      (insert "[debug] " (apply 'format fmt args))
      (newline))))

(defun twitter6-debug-mode ()
  (interactive)
  (setq twitter6-debug-mode
        (not twitter6-debug-mode))
  (message (if twitter6-debug-mode "debug mode:on" "debug mode:off")))

(defun twitter6-http-post-default-sentinel (proc stat &optional suc-msg)

  (condition-case err-signal
      (let ((header (twitter6-get-response-header))
            ;; (body (twitter6-get-response-body)) not used now.
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

(defun twitter6-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twitter6-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter6-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring content 0 (string-match "\r?\n\r?\n" content)))))

(defun twitter6-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a
XML tree as list. `buffer' may be a buffer or the name of an existing buffer. If
`buffer' is omitted, the value of `twitter6-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter6-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (let ((content (buffer-string)))
        (xml-parse-region (+ (string-match "\r?\n\r?\n" content)
                             (length (match-string 0 content)))
                          (point-max)))
      )))

(defun twitter6-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twitter6-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twitter6-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
            (not (find-if
                  (lambda (item)
                    (string= id (cdr (assq 'id item))))
                  (symbol-value data-var))))
        (progn
          (set data-var (cons status-datum (symbol-value data-var)))
          t)
      nil)))

(defun twitter6-status-to-status-datum (status)
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
      (setq text (twitter6-decode-html-entities
                  (assq-get 'text status-data)))
      (setq source (twitter6-decode-html-entities
                    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq in-reply-to-status-id
            (twitter6-decode-html-entities
             (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
            (twitter6-decode-html-entities
             (assq-get 'in_reply_to_screen_name status-data)))
      (setq user-id (assq-get 'id user-data))
      (setq user-name (twitter6-decode-html-entities
                       (assq-get 'name user-data)))
      (setq user-screen-name (twitter6-decode-html-entities
                              (assq-get 'screen_name user-data)))
      (setq user-location (twitter6-decode-html-entities
                           (assq-get 'location user-data)))
      (setq user-description (twitter6-decode-html-entities
                              (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
                    uri ,(concat "http://twitter.com/" user-screen-name)
                    face twitter6-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
                    uri ,(concat "http://twitter.com/" user-screen-name)
                    face twitter6-username-face)
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
                   face twitter6-uri-face
                   uri ,(concat "http://twitter.com/" screen-name))
               `(mouse-face highlight
                            face twitter6-uri-face
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
                          face twitter6-uri-face
                          source ,source)
             source)
            ))

      ;; save last update time
      (setq twitter6-timeline-last-update created-at)

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

(defun twitter6-xmltree-to-status (xmltree)
  (mapcar #'twitter6-status-to-status-datum
          ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
          ;; On Emacs22, there may be blank strings
          (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
            (while statuses
              (if (consp (car statuses))
                  (setq ret (cons (car statuses) ret)))
              (setq statuses (cdr statuses)))
            ret)))

(defun twitter6-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
          (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twitter6-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter6-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twitter6-decode-html-entities (encoded-str)
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
                     (twitter6-ucs-to-char
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

(defun twitter6-timer-action (func)
  (let ((buf (get-buffer twitter6-buffer)))
    (if (null buf)
        (twitter6-stop)
      (funcall func)
      )))

(defun twitter6-show-minibuffer-length (&optional beg end len)
  "Show the number of charactors in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
        (deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
           (status-len (- (buffer-size) (minibuffer-prompt-width)))
           (sign-len (length (twitter6-sign-string)))
           (mes (if (< 0 sign-len)
                    (format "%d=%d+%d"
                            (+ status-len sign-len) status-len sign-len)
                  (format "%d" status-len))))
      (if (<= 23 emacs-major-version)
          (minibuffer-message mes) ; Emacs23 or later
        (minibuffer-message (concat " (" mes ")")))
      )))

(defun twitter6-setup-minibuffer ()
  (add-hook 'post-command-hook 'twitter6-show-minibuffer-length t t))

(defun twitter6-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twitter6-show-minibuffer-length t))

(defun twitter6-status-not-blank-p (status)
  (with-temp-buffer
    (insert status)
    (goto-char (point-min))
    ;; skip user name
    (re-search-forward "@[-_a-z0-9]+\\([\n\r \t]+@[-_a-z0-9]+\\)*" nil t)
    (re-search-forward "[^\n\r \t]+" nil t)))

(defun twitter6-update-status-if-not-blank (status &optional reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (setq status (concat status (twitter6-sign-string)))
    (let ((parameters `(("status" . ,status)
                        ("source" . "twmode")
                        ,@(if reply-to-id
                              `(("in_reply_to_status_id"
                                 . ,reply-to-id))))))
      (twitter6-http-post "statuses" "update" parameters))
    t))

(defun twitter6-update-status-from-minibuffer (&optional init-str reply-to-id)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (when twitter6-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twitter6-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twitter6-finish-minibuffer t))
    (unwind-protect
      (while not-posted-p
        (setq status (read-from-minibuffer "status: " status nil nil nil nil t))
        (setq not-posted-p
          (not (twitter6-update-status-if-not-blank status reply-to-id))))
    (when (memq 'twitter6-setup-minibuffer minibuffer-setup-hook)
      (remove-hook 'minibuffer-setup-hook 'twitter6-setup-minibuffer))
    (when (memq 'twitter6-finish-minibuffer minibuffer-exit-hook)
      (remove-hook 'minibuffer-exit-hook 'twitter6-finish-minibuffer))
    )))

(defun twitter6-update-lambda ()
  (interactive)
  (twitter6-http-post
   "statuses" "update"
   `(("status" . "\xd34b\xd22b\xd26f\xd224\xd224\xd268\xd34b")
     ("source" . "twmode"))))

(defun twitter6-manage-favorites (method id)
  (twitter6-http-post-new "twitter.com"
                        (concat "favorites/" method "/" id)
                        `(("source" . "twmode"))))

;;;
;;; Commands
;;;

(defun twitter6-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twitter6-current-timeline-noninteractive))
  (if twitter6-timer
      nil
    (setq twitter6-timer
          (run-at-time "0 sec"
                       twitter6-timer-interval
                       #'twitter6-timer-action action))))

(defun twitter6-stop ()
  (interactive)
  (cancel-timer twitter6-timer)
  (setq twitter6-timer nil))

(defun twitter6-get-timeline (method)
  (if (not (eq twitter6-last-timeline-retrieved method))
      (setq twitter6-timeline-last-update nil
            twitter6-timeline-data nil))
  (setq twitter6-last-timeline-retrieved method)
  (let ((buf (get-buffer twitter6-buffer)))
    (if (not buf)
        (twitter6-stop)
      (if (not twitter6-timeline-last-update)
          (twitter6-http-get "statuses" method)
        (let* ((system-time-locale "C")
               (since
                (twitter6-global-strftime
                 "%a, %d %b %Y %H:%M:%S JST"
                 twitter6-timeline-last-update)))
          (twitter6-http-get "statuses" method
                               `(("since" . ,since)))))))

  (if (and twitter6-icon-mode window-system)
      (if twitter6-image-stack
          (let ((proc
                 (apply
                  #'start-process
                  "wget-images"
                  (twitter6-wget-buffer)
                  "wget"
                  (format "--directory-prefix=%s" twitter6-tmp-dir)
                  "--no-clobber"
                  "--quiet"
                  twitter6-image-stack)))
            (set-process-sentinel
             proc
             (lambda (proc stat)
               (clear-image-cache)
               (save-excursion
                 (set-buffer (twitter6-wget-buffer))
                 )))))))

(defun twitter6-friends-timeline ()
  (interactive)
  (twitter6-get-timeline "friends_timeline"))

(defun twitter6-replies-timeline ()
  (interactive)
  (twitter6-get-timeline "replies"))

(defun twitter6-public-timeline ()
  (interactive)
  (twitter6-get-timeline "public_timeline"))

(defun twitter6-user-timeline ()
  (interactive)
  (twitter6-get-timeline "user_timeline"))

(defun twitter6-current-timeline-interactive ()
  (interactive)
  (setq twitter6-last-timeline-interactive t)
  (twitter6-current-timeline))

(defun twitter6-current-timeline-noninteractive ()
  (setq twitter6-last-timeline-interactive nil)
  (twitter6-current-timeline))

(defun twitter6-current-timeline ()
  (if (not twitter6-last-timeline-retrieved)
      (setq twitter6-last-timeline-retrieved "friends_timeline"))
  (twitter6-get-timeline twitter6-last-timeline-retrieved))

(defun twitter6-update-status-interactive ()
  (interactive)
  (twitter6-update-status-from-minibuffer))

(defun twitter6-erase-old-statuses ()
  (interactive)
  (setq twitter6-timeline-data nil)
  (if (not twitter6-last-timeline-retrieved)
      (setq twitter6-last-timeline-retrieved "friends_timeline"))
  (if (not twitter6-timeline-last-update)
      (twitter6-http-get "statuses" twitter6-last-timeline-retrieved)
    (let* ((system-time-locale "C")
           (since
            (twitter6-global-strftime
             "%a, %d %b %Y %H:%M:%S JST"
             twitter6-timeline-last-update)))
      (twitter6-http-get "statuses" twitter6-last-timeline-retrieved
                           `(("since" . ,since))))))

(defun twitter6-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
        (browse-url uri))))

(defun twitter6-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (get-text-property (point) 'id))
        (uri (get-text-property (point) 'uri)))
    (if username
        (twitter6-update-status-from-minibuffer (concat "@" username " ") id)
      (if uri
          (browse-url uri)))))

(defun twitter6-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
        (browse-url uri))))

(defun twitter6-other-user-timeline ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
        (twitter6-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twitter6-other-user-timeline-interactive ()
  (interactive)
  (let ((username (read-from-minibuffer "user: " (get-text-property (point) 'username))))
    (if (> (length username) 0)
        (twitter6-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twitter6-favorite (&optional remove)
  (interactive)
  (let ((id (get-text-property (point) 'id))
        (text (get-text-property (point) 'text))
        (width (max 40 ;; XXX
                    (- (frame-width)
                       1 ;; margin for wide characters
                       15 ;; == (length (concat "Unfavorite \"" "\"? "))
                       9) ;; == (length "(y or n) ")
                    ))
        (method (if remove "destroy" "create")))
    (if id
        (let ((mes (format "%s \"%s\"? "
                           (if remove "Unfavorite" "Favorite")
                           (if (< width (string-width text))
                               (concat
                                (truncate-string-to-width text (- width 3))
                                "...")
                             text))))
          (when (y-or-n-p mes)
            (twitter6-manage-favorites method id)))
      (message "No status selected"))))

(defun twitter6-unfavorite
 ()
  (interactive)
  (twitter6-favorite t))

(defun twitter6-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
        (twitter6-update-status-from-minibuffer (concat "@" username " ")))))

(defun twitter6-get-password ()
  (or twitter6-password
      (setq twitter6-password (read-passwd "twitter6-mode: "))))

(defun twitter6-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twitter6-get-next-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (message "End of status."))))

(defun twitter6-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter6-username-face)))
        (setq pos (next-single-property-change pos 'face))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter6-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (twitter6-get-previous-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (message "Start of status."))))

(defun twitter6-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter6-username-face)))
        (setq pos (previous-single-property-change pos 'face))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter6-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twitter6-get-username-at-pos (point)))
        (pos (twitter6-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (twitter6-get-username-at-pos pos) user-name)))
      (setq pos (twitter6-get-next-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "End of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun twitter6-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twitter6-get-username-at-pos (point)))
        (pos (twitter6-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (twitter6-get-username-at-pos pos) user-name)))
      (setq pos (twitter6-get-previous-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "Start of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun twitter6-get-username-at-pos (pos)
  (let ((start-pos pos)
        (end-pos))
    (catch 'not-found
      (while (eq (get-text-property start-pos 'face) twitter6-username-face)
        (setq start-pos (1- start-pos))
        (when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun twitter6-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%s" username id))

;;;###autoload
(defun twit ()
  "Start twitter6-mode."
  (interactive)
  (twitter6-mode))

(provide 'twitter6-mode)
;;; twitter6.el ends here
