;;; twitter5-mode.el --- Major mode for Twitter

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

;; twitter5-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)

(defconst twitter5-mode-version "0.8")

(defun twitter5-mode-version ()
  "Display a message for twitter5-mode version."
  (interactive)
  (let ((version-string
         (format "twitter5-mode-v%s" twitter5-mode-version)))
    (if (interactive-p)
        (message "%s" version-string)
      version-string)))

(defconst twitter5-http-status-line-regexp
  "HTTP/1\.[01] \\(\\([0-9][0-9][0-9]\\) [^\r\n]+\\)\r?\n"
  "Regular expression used in \"sentinel\" functions to pick up
status-code and reason-phrase from the response.")

(defvar twitter5-mode-map (make-sparse-keymap))

(defvar twitter5-timer nil "Timer object for timeline refreshing will be
stored here. DO NOT SET VALUE MANUALLY.")

(defvar twitter5-idle-time 19)

(defvar twitter5-timer-interval 96)

(defvar twitter5-username nil)

(defvar twitter5-password nil)

(defvar twitter5-last-timeline-retrieved nil)

(defvar twitter5-last-timeline-interactive nil)

(defvar twitter5-new-tweets-count 0
  "Number of new tweets when `twitter5-new-tweets-hook' is run")

(defvar twitter5-new-tweets-hook nil
  "Hook run when new twits are received.

You can read `twitter5-new-tweets-count' to get the number of new
tweets received when this hook is run.")

(defvar twitter5-scroll-mode nil)
(make-variable-buffer-local 'twitter5-scroll-mode)

(defvar twitter5-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twitter5-status-format nil)
(setq twitter5-status-format "%i %s: %t %p [%C]")
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

(defvar twitter5-buffer "*twitter5*")
(defun twitter5-buffer ()
  (twitter5-get-or-generate-buffer twitter5-buffer))

(defvar twitter5-http-buffer "*twitter5-http-buffer*")
(defun twitter5-http-buffer ()
  (twitter5-get-or-generate-buffer twitter5-http-buffer))

(defvar twitter5-timeline-data nil)
(defvar twitter5-timeline-last-update nil)

(defvar twitter5-username-face 'twitter5-username-face)
(defvar twitter5-uri-face 'twitter5-uri-face)

(defun twitter5-get-or-generate-buffer (buffer)
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

(defvar twitter5-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)")

;;; Proxy
(defvar twitter5-proxy-use global-proxy-use)
(defvar twitter5-proxy-server global-proxy-server)
(defvar twitter5-proxy-port global-proxy-port)
(defvar twitter5-proxy-user global-proxy-user)
(defvar twitter5-proxy-password global-proxy-password)

(defun twitter5-toggle-proxy () ""
  (interactive)
  (setq twitter5-proxy-use
        (not twitter5-proxy-use))
  (message "%s %s"
           "Use Proxy:"
           (if twitter5-proxy-use
               "on" "off")))

(defun twitter5-user-agent-default-function ()
  "twitter5 mode default User-Agent function."
  (concat "Emacs/"
          (int-to-string emacs-major-version) "." (int-to-string
                                                   emacs-minor-version)
          " "
          "twitter5-mode/"
          twitter5-mode-version))

(defvar twitter5-sign-simple-string nil)

(defun twitter5-sign-string-default-function ()
  "Tweet append sign string:simple "
  (if twitter5-sign-simple-string
      (concat " [" twitter5-sign-simple-string "]")
    ""))

(defvar twitter5-user-agent-function 'twitter5-user-agent-default-function)
(defvar twitter5-sign-string-function 'twitter5-sign-string-default-function)

(defun twitter5-user-agent ()
  "Return User-Agent header string."
  (funcall twitter5-user-agent-function))

(defun twitter5-sign-string ()
  "Return Tweet sign string."
  (funcall twitter5-sign-string-function))

;;; to show image files

(defvar twitter5-wget-buffer "*twitter5-wget-buffer*")
(defun twitter5-wget-buffer ()
  (twitter5-get-or-generate-buffer twitter5-wget-buffer))

(defvar twitter5-tmp-dir
  (expand-file-name (concat "twmode-images-" (user-login-name))
                    temporary-file-directory))

(defvar twitter5-icon-mode nil "You MUST NOT CHANGE this variable
directory. You should change through function'twitter5-icon-mode'")

(make-variable-buffer-local 'twitter5-icon-mode)
(defun twitter5-icon-mode (&optional arg)
  (interactive)
  (setq twitter5-icon-mode
        (if twitter5-icon-mode
            (if (null arg)
                nil
              (> (prefix-numeric-value arg) 0))
          (when (or (null arg)
                    (and arg (> (prefix-numeric-value arg) 0)))
            (when (file-writable-p twitter5-tmp-dir)
              (progn
                (if (not (file-directory-p twitter5-tmp-dir))
                    (make-directory twitter5-tmp-dir))
                t)))))
  (twitter5-render-timeline))

(defun twitter5-scroll-mode (&optional arg)
  (interactive)
  (setq twitter5-scroll-mode
        (if (null arg)
            (not twitter5-scroll-mode)
          (> (prefix-numeric-value arg) 0))))

(defvar twitter5-image-stack nil)

(defun twitter5-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun twitter5-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
                      (apply 'encode-time (parse-time-string string))
                      uni))
(defun twitter5-local-strftime (fmt string)
  (twitter5-setftime fmt string nil))
(defun twitter5-global-strftime (fmt string)
  (twitter5-setftime fmt string t))

(defvar twitter5-debug-mode nil)
(defvar twitter5-debug-buffer "*debug*")
(defun twitter5-debug-buffer ()
  (twitter5-get-or-generate-buffer twitter5-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twitter5-debug-mode
           (with-current-buffer (twitter5-debug-buffer)
             (insert (prin1-to-string ,obsym))
             (newline)
             ,obsym)
         ,obsym))))

(defun twitter5-debug-mode ()
  (interactive)
  (setq twitter5-debug-mode
        (not twitter5-debug-mode))
  (message (if twitter5-debug-mode "debug mode:on" "debug mode:off")))

(if twitter5-mode-map
    (let ((km twitter5-mode-map))
      (define-key km "\C-c\C-f" 'twitter5-friends-timeline)
      (define-key km "e" 'twitter5-friends-timeline)
      (define-key km "\C-c\C-r" 'twitter5-replies-timeline)
      (define-key km "r" 'twitter5-replies-timeline)
      (define-key km "\C-c\C-g" 'twitter5-public-timeline)
      (define-key km "\C-c\C-u" 'twitter5-user-timeline)
      (define-key km "u" 'twitter5-user-timeline)
      (define-key km "\C-c\C-s" 'twitter5-update-status-interactive)
      (define-key km "w" 'twitter5-update-status-interactive)
      (define-key km "\C-c\C-d" 'twitter5-erase-old-statuses)
      (define-key km "\C-m" 'twitter5-enter)
      (define-key km [mouse-1] 'twitter5-click)
      (define-key km "\C-c\C-v" 'twitter5-view-user-page)
      (define-key km "g" 'twitter5-current-timeline)
      (define-key km "c" 'twitter5-current-timeline-interactive)
      (define-key km "i" 'twitter5-other-user-timeline)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twitter5-goto-next-status)
      (define-key km "k" 'twitter5-goto-previous-status)
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
      ;; (define-key km "n" 'twitter5-goto-next-status-of-user)
      ;; (define-key km "p" 'twitter5-goto-previous-status-of-user)
      (define-key km "n" 'windmove-down)
      (define-key km "p" 'windmove-up)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "\C-c\ i" 'twitter5-icon-mode)
      (define-key km "s" 'twitter5-scroll-mode)
      (define-key km "a" 'twitter5-favorite)
      (define-key km "q" 'twitter5-unfavorite)
      (define-key km "t" 'twitter5-toggle-proxy)
      (define-key km "\C-c\C-p" 'twitter5-toggle-proxy)
      nil))

(defvar twitter5-mode-syntax-table nil "")

(if twitter5-mode-syntax-table
    ()
  (setq twitter5-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twitter5-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twitter5-mode-syntax-table)
  )

(defun twitter5-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface twitter5-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twitter5-username-face)
  (set-face-attribute 'twitter5-username-face nil :underline t)
  (defface twitter5-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twitter5-uri-face nil :underline t)
  (add-to-list 'minor-mode-alist '(twitter5-icon-mode " tw-icon"))
  (add-to-list 'minor-mode-alist '(twitter5-scroll-mode " tw-scroll"))
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
(defmacro twitter5-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar twitter5-mode-string "twitter5 mode")

(defvar twitter5-mode-hook nil
  "twitter5-mode hook.")

(defun twitter5-mode ()
  "Major mode for Twitter
\\{twitter5-mode-map}"
  (interactive)
  (switch-to-buffer (twitter5-buffer))
  (kill-all-local-variables)
  (twitter5-mode-init-variables)
  (use-local-map twitter5-mode-map)
  (setq major-mode 'twitter5-mode)
  (setq mode-name twitter5-mode-string)
  (set-syntax-table twitter5-mode-syntax-table)
  (run-hooks 'twitter5-mode-hook)
  (font-lock-mode -1)
  (twitter5-start))

;;;
;;; Basic HTTP functions
;;;

(defun twitter5-start-http-session (method headers host port path parameters &optional noninteractive sentinel)
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
      (setq headers (cons `("User-Agent" . ,(twitter5-user-agent))
                          headers)))

  (twitter5-start-http-non-ssl-session
   method headers host port path parameters
   noninteractive sentinel)))

(defun twitter5-http-get (method-class method &optional parameters sentinel)
  (if (null sentinel) (setq sentinel 'twitter5-http-get-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter5-http-buffer))
    (erase-buffer))

  (let (proc server port
             (proxy-user twitter5-proxy-user)
             (proxy-password twitter5-proxy-password))
    (condition-case nil
        (progn
          (if (and twitter5-proxy-use twitter5-proxy-server)
              (setq server twitter5-proxy-server
                    port (if (integerp twitter5-proxy-port)
                             (int-to-string twitter5-proxy-port)
                           twitter5-proxy-port))
            (setq server "twitter.com"
                  port "80"))
          (setq proc
                (open-network-stream
                 "network-connection-process" (twitter5-http-buffer)
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
                                                (twitter5-percent-encode (car
                                                                            param-pair))
                                                (twitter5-percent-encode (cdr
                                                                            param-pair))))
                                      parameters
                                      "&")))
                           " HTTP/1.1" nl
                           "Host: twitter.com" nl
                           "User-Agent: " (twitter5-user-agent) nl
                           "Authorization: Basic "
                           (base64-encode-string
                            (concat twitter5-username ":"
                                    (twitter5-get-password)))
                           nl
                           "Accept: text/xml"
                           ",application/xml"
                           ",application/xhtml+xml"
                           ",application/html;q=0.9"
                           ",text/plain;q=0.8"
                           ",image/png,*/*;q=0.5" nl
                           "Accept-Charset: utf-8;q=0.7,*;q=0.7" nl
                           (when twitter5-proxy-use
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

(defun twitter5-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (twitter5-get-response-header))
        (body (twitter5-get-response-body))
        (status nil)
        )
    (if (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header)
        (progn
          (setq status (match-string-no-properties 1 header))
          (case-string
           status
           (("200 OK")
            (setq twitter5-new-tweets-count
                  (count t (mapcar
                            #'twitter5-cache-status-datum
                            (reverse (twitter5-xmltree-to-status
                                      body)))))
            (if (and (> twitter5-new-tweets-count 0)
                     (not twitter5-last-timeline-interactive))
                (run-hooks 'twitter5-new-tweets-hook))
            (setq twitter5-last-timeline-interactive t)
            (twitter5-render-timeline))
           (t (message status))))
      (message "Failure: Bad http response.")))
  )

(defun twitter5-render-timeline ()
  (with-current-buffer (twitter5-buffer)
    (let ((point (point))
          (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
              (insert (twitter5-format-status
                       status twitter5-status-format))
              (fill-region-as-paragraph
               (save-excursion (beginning-of-line) (point)) (point))
              (insert "\n"))
            twitter5-timeline-data)
      (if (and twitter5-image-stack window-system)
          (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twitter5-scroll-mode (- (point-max) end) 0))))
    ))

(defun twitter5-format-status (status format-str)
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
                  (if (file-exists-p (concat twitter5-tmp-dir
                                             "/" filename))
                      t
                    (add-to-list 'twitter5-image-stack profile-image-url))

                  (when (and icon-string twitter5-icon-mode)
                    (set-text-properties
                     1 2 `(display
                           (image :type ,(twitter5-image-type filename)
                                  :file ,(concat twitter5-tmp-dir
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
                     (url (twitter5-get-status-url reply-name reply-id)))
                 (add-text-properties
                  0 (length in-reply-to-string)
                  `(mouse-face highlight
                               face twitter5-uri-face
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
           (list-push (twitter5-local-strftime
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
               (setq url (twitter5-get-status-url (attr 'user-screen-name)
                                                    (attr 'id)))
               ;; make status url clickable
               (add-text-properties
                0 (length time-string)
                `(mouse-face highlight
                             face twitter5-uri-face
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

(defun twitter5-http-post
  (method-class method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com

METHOD-CLASS must be one of Twitter API method classes
 (statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twitter5-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twitter5-http-buffer))
    (erase-buffer))

  (let (proc server port
             (proxy-user twitter5-proxy-user)
             (proxy-password twitter5-proxy-password))
    (progn
      (if (and twitter5-proxy-use twitter5-proxy-server)
          (setq server twitter5-proxy-server
                port (if (integerp twitter5-proxy-port)
                         (int-to-string twitter5-proxy-port)
                       twitter5-proxy-port))
        (setq server "twitter.com"
              port "80"))
      (setq proc
            (open-network-stream
             "network-connection-process" (twitter5-http-buffer)
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
                                             (twitter5-percent-encode (car param-pair))
                                             (twitter5-percent-encode (cdr param-pair))))
                                   parameters
                                   "&")))
                        " HTTP/1.1" nl
                        "Host: twitter.com" nl
                        "User-Agent: " (twitter5-user-agent) nl
                        "Authorization: Basic "
                        (base64-encode-string
                         (concat twitter5-username ":" (twitter5-get-password)))
                        nl
                        "Content-Type: text/plain" nl
                        "Content-Length: 0" nl
                        (when twitter5-proxy-use
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

(defun twitter5-http-post-new (host method &optional parameters format sentinel)
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
      (setq sentinel 'twitter5-http-post-default-sentinel-new))

  (twitter5-start-http-session
   "POST" (twitter5-http-application-headers "POST")
   host nil (concat "/" method "." format) parameters noninteractive sentinel))

(defun twitter5-http-default-sentinel-new (func noninteractive proc stat &optional suc-msg)
  (debug-printf "http-default-sentinel: proc=%s stat=%s" proc stat)
  (let ((temp-buffer (process-buffer proc)))
    (unwind-protect
        (let ((header (twitter5-get-response-header temp-buffer))
              (mes nil))
          (if (string-match twitter5-http-status-line-regexp header)
              (when (and func (fboundp func))
                (with-current-buffer temp-buffer
                  (setq mes (funcall func header proc noninteractive suc-msg))))
            (setq mes "Failure: Bad http response."))
          (when (and mes (twitter5-buffer-active-p))
            (message mes)))
      ;; unwindforms
      (twitter5-release-process proc)
      (when (and (not twitter5-debug-mode) (buffer-live-p temp-buffer))
        (kill-buffer temp-buffer))))
  )

(defun twitter5-start-http-non-ssl-session (method headers host port path parameters &optional noninteractive sentinel)
  (let ((request (twitter5-make-http-request
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
             (server (if twitter5-proxy-use
                         twitter5-proxy-server
                       (request :host)))
             (port (if twitter5-proxy-use
                       twitter5-proxy-port
                     (request :port)))
             (proc (open-network-stream
                    "network-connection-process" temp-buffer server port))
             )
        (lexical-let ((sentinel sentinel)
                      (noninteractive noninteractive))
          (set-process-sentinel
           proc
           (lambda (&rest args)
             (apply #'twitter5-http-default-sentinel
                    sentinel noninteractive args))))
        (debug-print request-str)
        (process-send-string proc request-str)
        proc)))
  )

(defun twitter5-make-http-request (method headers host port path parameters)
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
                        (twitter5-percent-encode (car pair))
                        (twitter5-percent-encode (cdr pair))))
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

(defun twitter5-http-application-headers (&optional method headers)
  "Retuns an assoc list of HTTP headers for twitter5-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twitter5-user-agent)) headers)
    (push (cons "Authorization"
                (concat "Basic "
                        (base64-encode-string
                         (concat
                          twitter5-username
                          ":"
                          (twitter5-get-password)))))
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
                        (when twitter5-proxy-use
                          "Proxy-Connection: Keep-Alive" headers
                          (when (and twitter5-proxy-user twitter5-proxy-password)
                            (concat
                             "Proxy-Authorization: Basic "
                             (base64-encode-string
                              (concat twitter5-proxy-user ":"
                                      twitter5-proxy-password))
              headers)))
    headers
    ))

(defun twitter5-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
          (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twitter5-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%02x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter5-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?Z))
      (and (<= ?a ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twitter5-http-default-sentinel (func noninteractive proc stat &optional suc-msg)
  (debug-printf "http-default-sentinel: proc=%s stat=%s" proc stat)
  (let ((temp-buffer (process-buffer proc)))
    (unwind-protect
        (let ((header (twitter5-get-response-header temp-buffer))
              (mes nil))
          (if (string-match twitter5-http-status-line-regexp header)
              (when (and func (fboundp func))
                (with-current-buffer temp-buffer
                  (setq mes (funcall func header proc noninteractive suc-msg))))
            (setq mes "Failure: Bad http response."))
          (when (and mes (twitter5-buffer-active-p))
            (message mes)))
      ;; unwindforms
      (twitter5-release-process proc)
      (when (and (not twitter5-debug-mode) (buffer-live-p temp-buffer))
        (kill-buffer temp-buffer))))
  )

(defun twitter5-release-process (proc)
  (let ((spec (twitter5-get-timeline-spec-from-process proc)))
    (setq twitter5-process-info-alist
          (delete `(,proc ,spec) twitter5-process-info-alist))))

(defun twitter5-get-timeline-spec-from-process (proc)
  (let ((entry (assoc proc twitter5-process-info-alist)))
    (if entry
        (elt entry 1)
      nil)))

(defun debug-printf (fmt &rest args)
  (when twitter5-debug-mode
    (with-current-buffer (twitter5-debug-buffer)
      (insert "[debug] " (apply 'format fmt args))
      (newline))))

(defun twitter5-debug-mode ()
  (interactive)
  (setq twitter5-debug-mode
        (not twitter5-debug-mode))
  (message (if twitter5-debug-mode "debug mode:on" "debug mode:off")))

(defun twitter5-http-post-default-sentinel (proc stat &optional suc-msg)

  (condition-case err-signal
      (let ((header (twitter5-get-response-header))
            ;; (body (twitter5-get-response-body)) not used now.
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

(defun twitter5-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twitter5-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter5-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring content 0 (string-match "\r?\n\r?\n" content)))))

(defun twitter5-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a
XML tree as list. `buffer' may be a buffer or the name of an existing buffer. If
`buffer' is omitted, the value of `twitter5-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twitter5-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (let ((content (buffer-string)))
        (xml-parse-region (+ (string-match "\r?\n\r?\n" content)
                             (length (match-string 0 content)))
                          (point-max)))
      )))

(defun twitter5-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twitter5-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twitter5-timeline-data))
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

(defun twitter5-status-to-status-datum (status)
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
      (setq text (twitter5-decode-html-entities
                  (assq-get 'text status-data)))
      (setq source (twitter5-decode-html-entities
                    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq in-reply-to-status-id
            (twitter5-decode-html-entities
             (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
            (twitter5-decode-html-entities
             (assq-get 'in_reply_to_screen_name status-data)))
      (setq user-id (assq-get 'id user-data))
      (setq user-name (twitter5-decode-html-entities
                       (assq-get 'name user-data)))
      (setq user-screen-name (twitter5-decode-html-entities
                              (assq-get 'screen_name user-data)))
      (setq user-location (twitter5-decode-html-entities
                           (assq-get 'location user-data)))
      (setq user-description (twitter5-decode-html-entities
                              (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
                    uri ,(concat "http://twitter.com/" user-screen-name)
                    face twitter5-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
                    uri ,(concat "http://twitter.com/" user-screen-name)
                    face twitter5-username-face)
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
                   face twitter5-uri-face
                   uri ,(concat "http://twitter.com/" screen-name))
               `(mouse-face highlight
                            face twitter5-uri-face
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
                          face twitter5-uri-face
                          source ,source)
             source)
            ))

      ;; save last update time
      (setq twitter5-timeline-last-update created-at)

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

(defun twitter5-xmltree-to-status (xmltree)
  (mapcar #'twitter5-status-to-status-datum
          ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
          ;; On Emacs22, there may be blank strings
          (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
            (while statuses
              (if (consp (car statuses))
                  (setq ret (cons (car statuses) ret)))
              (setq statuses (cdr statuses)))
            ret)))

(defun twitter5-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
          (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twitter5-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter5-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twitter5-decode-html-entities (encoded-str)
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
                     (twitter5-ucs-to-char
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

(defun twitter5-timer-action (func)
  (let ((buf (get-buffer twitter5-buffer)))
    (if (null buf)
        (twitter5-stop)
      (funcall func)
      )))

(defun twitter5-show-minibuffer-length (&optional beg end len)
  "Show the number of charactors in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
        (deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
           (status-len (- (buffer-size) (minibuffer-prompt-width)))
           (sign-len (length (twitter5-sign-string)))
           (mes (if (< 0 sign-len)
                    (format "%d=%d+%d"
                            (+ status-len sign-len) status-len sign-len)
                  (format "%d" status-len))))
      (if (<= 23 emacs-major-version)
          (minibuffer-message mes) ; Emacs23 or later
        (minibuffer-message (concat " (" mes ")")))
      )))

(defun twitter5-setup-minibuffer ()
  (add-hook 'post-command-hook 'twitter5-show-minibuffer-length t t))

(defun twitter5-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twitter5-show-minibuffer-length t))

(defun twitter5-status-not-blank-p (status)
  (with-temp-buffer
    (insert status)
    (goto-char (point-min))
    ;; skip user name
    (re-search-forward "@[-_a-z0-9]+\\([\n\r \t]+@[-_a-z0-9]+\\)*" nil t)
    (re-search-forward "[^\n\r \t]+" nil t)))

(defun twitter5-update-status-if-not-blank (status &optional reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (setq status (concat status (twitter5-sign-string)))
    (let ((parameters `(("status" . ,status)
                        ("source" . "twmode")
                        ,@(if reply-to-id
                              `(("in_reply_to_status_id"
                                 . ,reply-to-id))))))
      (twitter5-http-post "statuses" "update" parameters))
    t))

(defun twitter5-update-status-from-minibuffer (&optional init-str reply-to-id)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (when twitter5-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twitter5-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twitter5-finish-minibuffer t))
    (unwind-protect
      (while not-posted-p
        (setq status (read-from-minibuffer "status: " status nil nil nil nil t))
        (setq not-posted-p
          (not (twitter5-update-status-if-not-blank status reply-to-id))))
    (when (memq 'twitter5-setup-minibuffer minibuffer-setup-hook)
      (remove-hook 'minibuffer-setup-hook 'twitter5-setup-minibuffer))
    (when (memq 'twitter5-finish-minibuffer minibuffer-exit-hook)
      (remove-hook 'minibuffer-exit-hook 'twitter5-finish-minibuffer))
    )))

(defun twitter5-update-lambda ()
  (interactive)
  (twitter5-http-post
   "statuses" "update"
   `(("status" . "\xd34b\xd22b\xd26f\xd224\xd224\xd268\xd34b")
     ("source" . "twmode"))))

(defun twitter5-manage-favorites (method id)
  (twitter5-http-post-new "twitter.com"
                        (concat "favorites/" method "/" id)
                        `(("source" . "twmode"))))

;;;
;;; Commands
;;;

(defun twitter5-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twitter5-current-timeline-noninteractive))
  (if twitter5-timer
      nil
    (setq twitter5-timer
          (run-at-time "0 sec"
                       twitter5-timer-interval
                       #'twitter5-timer-action action))))

(defun twitter5-stop ()
  (interactive)
  (cancel-timer twitter5-timer)
  (setq twitter5-timer nil))

(defun twitter5-get-timeline (method)
  (if (not (eq twitter5-last-timeline-retrieved method))
      (setq twitter5-timeline-last-update nil
            twitter5-timeline-data nil))
  (setq twitter5-last-timeline-retrieved method)
  (let ((buf (get-buffer twitter5-buffer)))
    (if (not buf)
        (twitter5-stop)
      (if (not twitter5-timeline-last-update)
          (twitter5-http-get "statuses" method)
        (let* ((system-time-locale "C")
               (since
                (twitter5-global-strftime
                 "%a, %d %b %Y %H:%M:%S JST"
                 twitter5-timeline-last-update)))
          (twitter5-http-get "statuses" method
                               `(("since" . ,since)))))))

  (if (and twitter5-icon-mode window-system)
      (if twitter5-image-stack
          (let ((proc
                 (apply
                  #'start-process
                  "wget-images"
                  (twitter5-wget-buffer)
                  "wget"
                  (format "--directory-prefix=%s" twitter5-tmp-dir)
                  "--no-clobber"
                  "--quiet"
                  twitter5-image-stack)))
            (set-process-sentinel
             proc
             (lambda (proc stat)
               (clear-image-cache)
               (save-excursion
                 (set-buffer (twitter5-wget-buffer))
                 )))))))

(defun twitter5-friends-timeline ()
  (interactive)
  (twitter5-get-timeline "friends_timeline"))

(defun twitter5-replies-timeline ()
  (interactive)
  (twitter5-get-timeline "replies"))

(defun twitter5-public-timeline ()
  (interactive)
  (twitter5-get-timeline "public_timeline"))

(defun twitter5-user-timeline ()
  (interactive)
  (twitter5-get-timeline "user_timeline"))

(defun twitter5-current-timeline-interactive ()
  (interactive)
  (setq twitter5-last-timeline-interactive t)
  (twitter5-current-timeline))

(defun twitter5-current-timeline-noninteractive ()
  (setq twitter5-last-timeline-interactive nil)
  (twitter5-current-timeline))

(defun twitter5-current-timeline ()
  (if (not twitter5-last-timeline-retrieved)
      (setq twitter5-last-timeline-retrieved "friends_timeline"))
  (twitter5-get-timeline twitter5-last-timeline-retrieved))

(defun twitter5-update-status-interactive ()
  (interactive)
  (twitter5-update-status-from-minibuffer))

(defun twitter5-erase-old-statuses ()
  (interactive)
  (setq twitter5-timeline-data nil)
  (if (not twitter5-last-timeline-retrieved)
      (setq twitter5-last-timeline-retrieved "friends_timeline"))
  (if (not twitter5-timeline-last-update)
      (twitter5-http-get "statuses" twitter5-last-timeline-retrieved)
    (let* ((system-time-locale "C")
           (since
            (twitter5-global-strftime
             "%a, %d %b %Y %H:%M:%S JST"
             twitter5-timeline-last-update)))
      (twitter5-http-get "statuses" twitter5-last-timeline-retrieved
                           `(("since" . ,since))))))

(defun twitter5-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
        (browse-url uri))))

(defun twitter5-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (get-text-property (point) 'id))
        (uri (get-text-property (point) 'uri)))
    (if username
        (twitter5-update-status-from-minibuffer (concat "@" username " ") id)
      (if uri
          (browse-url uri)))))

(defun twitter5-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
        (browse-url uri))))

(defun twitter5-other-user-timeline ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if (> (length username) 0)
        (twitter5-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twitter5-other-user-timeline-interactive ()
  (interactive)
  (let ((username (read-from-minibuffer "user: " (get-text-property (point) 'username))))
    (if (> (length username) 0)
        (twitter5-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twitter5-favorite (&optional remove)
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
            (twitter5-manage-favorites method id)))
      (message "No status selected"))))

(defun twitter5-unfavorite
 ()
  (interactive)
  (twitter5-favorite t))

(defun twitter5-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
        (twitter5-update-status-from-minibuffer (concat "@" username " ")))))

(defun twitter5-get-password ()
  (or twitter5-password
      (setq twitter5-password (read-passwd "twitter5-mode: "))))

(defun twitter5-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twitter5-get-next-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (message "End of status."))))

(defun twitter5-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter5-username-face)))
        (setq pos (next-single-property-change pos 'face))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter5-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (twitter5-get-previous-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (message "Start of status."))))

(defun twitter5-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twitter5-username-face)))
        (setq pos (previous-single-property-change pos 'face))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face)))
      pos)))

(defun twitter5-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twitter5-get-username-at-pos (point)))
        (pos (twitter5-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (twitter5-get-username-at-pos pos) user-name)))
      (setq pos (twitter5-get-next-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "End of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun twitter5-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twitter5-get-username-at-pos (point)))
        (pos (twitter5-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (twitter5-get-username-at-pos pos) user-name)))
      (setq pos (twitter5-get-previous-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "Start of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun twitter5-get-username-at-pos (pos)
  (let ((start-pos pos)
        (end-pos))
    (catch 'not-found
      (while (eq (get-text-property start-pos 'face) twitter5-username-face)
        (setq start-pos (1- start-pos))
        (when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun twitter5-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%s" username id))

;;;###autoload
(defun twit ()
  "Start twitter5-mode."
  (interactive)
  (twitter5-mode))

(provide 'twitter5-mode)
;;; twitter5.el ends here
