;;; web-vcs.el --- Download file trees from VCS web pages
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-11-26 Thu
(defconst web-vcs:version "0.6") ;; Version:
;; Last-Updated: 2009-12-01 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Update file trees within Emacs from VCS systems using information
;; on their web pages.
;;
;; See the example `nxhtml-download'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defcustom web-vcs-links-regexp
  `(
    (lp
     "http://www.launchpad.com/ uses this 2009-11-29\nwith Loggerhead 1.10, generic?"
     ,(rx "href=\""
          (submatch
           (regexp ".*/download/[^\"]*"))
          "\"")
     ,(rx "href=\""
          (submatch
           (regexp ".*%3A/[^\"]*/"))
          "\"")
     "\\([^\/]*\\)$"
     ,(rx "for revision"
          (+ whitespace)
          "<span>"
          (submatch (+ digit))
          "</span>")
     ;;"/head%3A/\\([^/]*\\)/$"
     )
    )
  "Regexp patterns for matching links on a VCS web page.
The patterns are grouped by VCS web system type.

*Note: It is always sub match 1 from these patterns that are
       used."
  :type '(repeat
          (list
           (symbol :tag "VCS web system type specifier")
           (string :tag "Description")
           (regexp :tag "Files URL regexp")
           (regexp :tag "Dirs URL regexp")
           (regexp :tag "File name URL part regexp")
           (regexp :tag "Revision regexp")
           ;;(regexp :tag "Subdir regexp")
           ))
  :group 'web-vcs)



;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/" t "c:/test/temp13/" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/util/" t "temp" t)
;; (web-vcs-get-files-on-page 'lp "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/alts/" t "temp" t)


;;;###autoload
(defun web-vcs-get-files-from-root (web-vcs url dl-dir)
  "Download a file tree from VCS system using the web interface.
Use WEB-VCS entry in variable `web-vcs-links-regexp' to download
files via http from URL to directory DL-DIR.

Show URL first and offer to visit the page.  That page will give
you information about version control system \(VCS) system used
etc."
  (unless (web-vcs-contains-moved-files dl-dir)
    (when (if (not (y-or-n-p (concat "Download files from \"" url "\".\n"
                                     "You can see on that page which files will be downloaded.\n\n"
                                     "Visit that page before downloading? ")))
              t
            (browse-url url)
            (if (y-or-n-p "Start downloading? ")
                t
              (message "Aborted")
              nil))
      (message "")
      (web-vcs-get-files-on-page web-vcs url t (file-name-as-directory dl-dir) nil))))


(defun web-vcs-get-files-on-page (web-vcs url recursive dl-dir test)
  "Download files listed by WEB-VCS on web page URL.
WEB-VCS is a specifier in `web-vcs-links-regexp'.

If RECURSIVE go into sub folders on the web page and download
files from them too.

Place the files under DL-DIR.

Before downloading check if the downloaded revision already is
the same as the one on the web page.  This is stored in the file
web-vcs-revision.txt.  After downloading update this file.

If TEST is non-nil then do not download, just list the files."
  (require 'hi-lock) ;; For faces
  (unless (string= dl-dir (file-name-as-directory (expand-file-name dl-dir)))
    (error "Download dir dl-dir=%S must be a full directory path" dl-dir))
  (catch 'command-level
    (when (web-vcs-contains-moved-files dl-dir)
      (throw 'command-level nil))
    (let ((vcs-rec (or (assq web-vcs web-vcs-links-regexp)
                       (error "Does not know web-cvs %S" web-vcs)))
          (start-time (current-time)))
      (unless (file-directory-p dl-dir)
        (if (yes-or-no-p (format "Directory %S does not exist, create it? " (expand-file-name dl-dir)))
            (mkdir dl-dir t)
          (message "Can't download then")
          (throw 'command-level nil)))
      (let ((old-win (selected-window)))
        (unless (eq (get-buffer "*Messages*") (window-buffer old-win))
          (switch-to-buffer-other-window "*Messages*"))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (format "\n\nWeb-Vcs Download: %S\n" url) 'face 'hi-gold))
        (insert "\n")
        (redisplay t)
        (set-window-point (selected-window) (point-max))
        (select-window old-win))
      (let* ((rev-file (expand-file-name "web-vcs-revision.txt" dl-dir))
             (rev-buf (find-file-noselect rev-file))
             ;; Fix-me: Per web vcs speficier.
             (old-rev-range (with-current-buffer rev-buf
                              (widen)
                              (goto-char (point-min))
                              (when (re-search-forward (format "%s:\\(.*\\)\n" web-vcs) nil t)
                                ;;(buffer-substring-no-properties (point-min) (line-end-position))
                                ;;(match-string 1)
                                (cons (match-beginning 1) (match-end 1))
                                )))
             (old-revision (when old-rev-range
                             (with-current-buffer rev-buf
                               (buffer-substring-no-properties (car old-rev-range)
                                                               (cdr old-rev-range)))))
             (dl-revision (web-vcs-get-revision-on-page vcs-rec url))
             ret
             moved)
        (when (and old-revision (string= old-revision dl-revision))
          (when (y-or-n-p (format "You already have revision %s.  Quit? " dl-revision))
            (message "Aborted")
            (kill-buffer rev-buf)
            (throw 'command-level nil)))
        ;; We do not have a revision number once we start download.
        (with-current-buffer rev-buf
          (when old-rev-range
            (delete-region (car old-rev-range) (cdr old-rev-range))
            (basic-save-buffer)))
        (setq ret (web-vcs-get-files-on-page-1
                   vcs-rec url (if recursive 0 nil) dl-dir dl-revision test))
        (setq moved       (nth 1 ret))
        ;; Now we have a revision number again.
        (with-current-buffer rev-buf
          (when (= 0 (buffer-size))
            (insert "WEB VCS Revisions\n\n"))
          (goto-char (point-max))
          (unless (eolp) (insert "\n"))
          (insert (format "%s:%s\n" web-vcs dl-revision))
          (basic-save-buffer)
          (kill-buffer))
        (message "-----------------")
        (if (> moved 0)
            (web-vcs-message-with-face 'hi-yellow
                                       "Download ready, %s. %i files updated (old versions renamed to *.moved)"
                                       (web-vcs-nice-elapsed start-time (current-time))
                                       moved)
          (web-vcs-message-with-face 'hi-green
                                     "Download ready, %s"
                                     (web-vcs-nice-elapsed start-time (current-time))))))))


(defun web-vcs-get-files-on-page-1 (vcs-rec url recursive dl-dir dl-revision test)
  "Download files listed by WEB-VCS on web page URL.
VCS-REC should be an entry like the entries in the list
`web-vcs-links-regexp'.

If RECURSIVE go into sub folders on the web page and download
files from them too.

Place the files under DL-DIR.

If TEST is non-nil then do not download, just list the files."
  (let* ((files-href-regexp  (nth 2 vcs-rec))
         (dirs-href-regexp   (nth 3 vcs-rec))
         (file-name-regexp   (nth 4 vcs-rec))
         (revision-regexp    (nth 5 vcs-rec))
         (url-buf (url-retrieve-synchronously url))
         this-page-revision
         files
         suburls
         (moved 0)
         (temp-file (expand-file-name "web-vcs-temp.tmp" dl-dir)))
    (with-current-buffer url-buf
      (goto-char (point-min))
      (unless (looking-at "HTTP/.* 200 OK\n")
        (switch-to-buffer url-buf)
        (error "Download error: %S" url))
      (unless (file-directory-p dl-dir)
        (make-directory dl-dir t))
      ;; Get revision number
      (setq this-page-revision (web-vcs-get-revision-from-url-buf vcs-rec url url-buf))
      (unless (string= dl-revision this-page-revision)
        (web-vcs-message-with-face 'hi-salmon "Revision on %S is %S, but should be %S"
                                   url this-page-revision dl-revision)
        (throw 'command-level nil))
      ;; Find files
      (goto-char (point-min))
      (while (re-search-forward files-href-regexp nil t)
        (add-to-list 'files (match-string 1)))
      ;; Find subdirs
      (when recursive
        (goto-char (point-min))
        (while (re-search-forward dirs-href-regexp nil t)
          (let ((suburl (match-string 1))
                (lenurl (length url)))
            (when (and (> (length suburl) lenurl)
                       (string= (substring suburl 0 lenurl) url))
              (add-to-list 'suburls suburl)))))
      (kill-buffer))
    ;; Download files
    (dolist (file (reverse files))
      (let* ((file-url file)
             (file-name (progn
                          (when (string-match file-name-regexp file-url)
                            (match-string 1 file-url))))
             (file-dl-name (expand-file-name file-name dl-dir))
             temp-buf
             )
        (if test
            (progn
              (message "TEST file-url=%S" file-url)
              (message "TEST file-name=%S" file-name)
              (message "TEST file-dl-name=%S" file-dl-name)
              )
          (while (setq temp-buf (find-buffer-visiting temp-file))
            (set-buffer-modified-p nil)
            (kill-buffer temp-buf))
          ;; Use url-copy-file, this takes care of coding system.
          (url-copy-file file-url temp-file t t) ;; overwrite, keep time
          (let* (;; (new-buf (find-file-noselect temp-file))
                 ;; (new-src (with-current-buffer new-buf
                 ;;            (save-restriction
                 ;;              (widen)
                 ;;              (buffer-substring-no-properties (point-min) (point-max)))))
                 (time-after-url-copy (current-time))
                 (old-exists (file-exists-p file-dl-name))
                 (old-buf-open (find-buffer-visiting file-dl-name))
                 ;; (old-buf (or old-buf-open
                 ;;              (when old-exists
                 ;;                (let ((auto-mode-alist nil))
                 ;;                  (find-file-noselect file-dl-name)))))
                 ;; old-src
                 )
            (when old-buf-open
              (when (buffer-modified-p old-buf-open)
                (save-excursion
                  (switch-to-buffer old-buf-open)
                  (when (y-or-n-p (format "Buffer %S is modified, save to make a backup? "
                                          file-dl-name))
                    (save-buffer)))))
            ;;(if (and old-src (string= new-src old-src))
            (if (and old-exists
                     (web-vcs-equal-files file-dl-name temp-file))
                (web-vcs-message-with-face 'hi-green "File %S was ok" file-dl-name)
              (when old-exists
                (let ((backup (concat file-dl-name ".moved")))
                  (when (file-exists-p backup)
                    (delete-file backup))
                  (rename-file file-dl-name backup)))
              (rename-file temp-file file-dl-name)
              (if old-exists
                  (web-vcs-message-with-face 'hi-yellow "Updated %S" file-dl-name)
                (web-vcs-message-with-face 'hi-green "Downloaded %S" file-dl-name))
              (when old-buf-open
                (with-current-buffer old-buf-open
                  (set-buffer-modified-p nil)
                  (revert-buffer))))
            (let* ((msg-win (get-buffer-window "*Messages*")))
              (with-current-buffer "*Messages*"
                (set-window-point msg-win (point-max))))
            (redisplay t)
            ;; This is both for user and remote server load.  Do not remove this.
            (sit-for (- 1.0 (float-time (time-subtract (current-time) time-after-url-copy))))
            ;; (unless old-buf-open
            ;;   (when old-buf
            ;;     (kill-buffer old-buf)))
            ))
        (redisplay t)))
    ;; Download subdirs
    (when suburls
      (dolist (suburl (reverse suburls))
        (let* ((dl-sub-dir (substring suburl (length url)))
               (full-dl-sub-dir (file-name-as-directory
                                 (expand-file-name dl-sub-dir dl-dir))))
          (unless (web-vcs-contains-file dl-dir full-dl-sub-dir)
            (error "Subdir %S not in %S" dl-sub-dir dl-dir))
          (let* ((ret (web-vcs-get-files-on-page-1 vcs-rec
                                                  suburl
                                                  (1+ recursive)
                                                  full-dl-sub-dir
                                                  this-page-revision
                                                  test)))
            (setq moved (+ moved (nth 1 ret)))
            ))))
    (list this-page-revision moved)
    ))


(defun web-vcs-get-revision-on-page (vcs-rec url)
  "Get revision number using VCS-REC on page URL.
VCS-REC should be an entry like the entries in the list
`web-vcs-links-regexp'."
  (let ((url-buf (url-retrieve-synchronously url)))
    (web-vcs-get-revision-from-url-buf vcs-rec url url-buf)))

(defun web-vcs-get-revision-from-url-buf (vcs-rec url url-buf)
  (let ((revision-regexp    (nth 5 vcs-rec)))
    ;; Get revision number
    (with-current-buffer url-buf
      (goto-char (point-min))
      (if (not (re-search-forward revision-regexp nil t))
          (progn
            (web-vcs-message-with-face 'hi-salmon "Can't find revision number on %S" url)
            (throw 'command-level nil))
        (match-string 1)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun web-vcs-contains-file (dir file)
  (assert (string= dir (file-name-as-directory (expand-file-name dir))) t)
  (assert (or (string= file (file-name-as-directory (expand-file-name file)))
              (string= file (expand-file-name file))) t)
  (let ((dir-len (length dir)))
    (assert (string= "/" (substring dir (1- dir-len))))
    (when (> (length file) dir-len)
      (string= dir (substring file 0 dir-len)))))

(defun web-vcs-nice-elapsed (start-time end-time)
  "Format elapsed time between START-TIME and END-TIME nicely.
Those times should have the same format as time returned by
`current-time'."
  (format-seconds "%h h %m m %z%s s" (float-time (time-subtract end-time start-time))))

;; (web-vcs-equal-files "web-vcs.el" "temp.tmp")
;; (web-vcs-equal-files "../.nosearch" "temp.tmp")
(defun web-vcs-equal-files (file-a file-b)
  "Return t if files FILE-A and FILE-B are equal."
  (let* ((cmd (if (eq system-type 'windows-nt)
                  (list "fc" nil nil nil
                        "/B" "/OFF"
                        (convert-standard-filename file-a)
                        (convert-standard-filename file-b))
                (list diff-command nil nil nil
                      "--binary" "-q" file-a file-b)))
         (ret (apply 'call-process cmd)))
    ;;(message "ret=%s, cmd=%S" ret cmd) (sit-for 2)
    (cond
     ((= 1 ret)
      nil)
     ((= 0 ret)
      t)
     (t
      (error "%S returned %d" cmd ret)))))

;; (web-vcs-message-with-face 'secondary-selection "I am saying: %s and %s" "Hi" "Farwell!")
(defun web-vcs-message-with-face (face format-string &rest args)
  "Display a colored message at the bottom of the string.
FACE is the face to use for the message.
FORMAT-STRING and ARGS are the same as for `message'.

Also put FACE on the message in *Messages* buffer."
  (with-current-buffer "*Messages*"
    (save-restriction
      (widen)
      (let* ((start (let ((here (point)))
                      (goto-char (point-max))
                      (prog1
                          (if (bolp) (point-max)
                            (1+ (point-max)))
                        (goto-char here))))
             (msg-with-face (propertize (apply 'format format-string args)
                                        'face face)))
        ;; This is for the echo area:
        (message "%s" msg-with-face)
        ;; This is for the buffer:
        (goto-char (point-max))
        (backward-char)
        (unless (eolp) (goto-char (line-end-position)))
        (put-text-property start (point)
                           'face face)))))

;; (web-vcs-num-moved "c:/emacs/p/091105/EmacsW32/nxhtml/")
(defun web-vcs-num-moved (root)
  "Return nof files matching *.moved inside directory ROOT."
  (let* ((file-regexp ".*\\.moved$")
        (files (directory-files root t file-regexp))
        (subdirs (directory-files root t)))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (rdir-get-files subdir file-regexp) nil))))
    (length files)))

(defun web-vcs-contains-moved-files (dl-dir)
  "Return t if there are *.moved files in DL-DIR."
  (let ((num-moved (web-vcs-num-moved dl-dir)))
    (when (> num-moved 0)
      (web-vcs-message-with-face 'font-lock-warning-face
                                 (concat "There are %d *.moved files (probably from prev download)\n"
                                         "in %S.\nPlease delete them first.")
                                 num-moved dl-dir)
        t)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific

;;(call-interactively 'nxhtml-download)
;;;###autoload
(defun nxhtml-download ()
  "Download or update nXhtml.
If you already have nXhtml installed you can update it with this
command.  Otherwise after downloading read the instructions in
README.txt in the download directory for setting up nXhtml.
\(This requires adding only one line to your .emacs, but you may
optionally also byte compile the files from the nXhtml menu.)

To learn more about nXhtml visit its home page at URL
`http://www.emacswiki.com/NxhtmlMode/'."
  (interactive)
  (let ((msg (concat "Downloading nXhtml through Launchpad web interface will take rather long\n"
                     "time (5-15 minutes) so you may want to do it in a separate Emacs session.\n\n"
                     "Do you want to download using this Emacs session? "
                     )))
    (if (not (y-or-n-p msg))
        (message "Aborted")
      (message "")
      (let* ((dl-dir (or (when (and (boundp 'nxhtml-install-dir)
                                    nxhtml-install-dir
                                    (yes-or-no-p
                                     (format "Update current nXhtml files (%s)? "
                                             nxhtml-install-dir)))
                           nxhtml-install-dir)
                         (read-directory-name "Download nXhtml to: ")))
             (revision nil)
             (do-byte (when (string= dl-dir nxhtml-install-dir)
                        (y-or-n-p "Do you want to byte compile the files after downloading? "))))
        ;; http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/322
        ;; http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/head%3A/"
        (nxhtml-download-1 dl-dir revision do-byte)))))

(defun nxhtml-download-1 (dl-dir revision do-byte)
  "Download nXhtml to directory DL-DIR.
If REVISION is nil download latest revision, otherwise the
specified one."
  (let* ((base-link "http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/files/")
         (rev-link (if revision (number-to-string revision) "head%3A/"))
         (full-link (concat base-link rev-link)))
    (when (web-vcs-get-files-from-root 'lp full-link dl-dir)
      (when do-byte
        (sit-for 10)
        (web-vcs-message-with-face 'hi-yellow "Will start byte compilation of nXhtml in 10 seconds")
        (sit-for 10)
        (nxhtmlmaint-start-byte-compilation)))))


(provide 'web-vcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-vcs.el ends here
