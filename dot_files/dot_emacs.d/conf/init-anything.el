;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'anything-config)

(setq anything-c-adaptive-history-file
      (expand-file-name "~/.emacs.d/anything/anything-c-adaptive-history"))

(define-key global-map "\C-l" 'anything)
(define-key anything-map "\C-o" 'anything-next-source)
(define-key anything-map "\C-\M-n" 'anything-next-source)
(define-key anything-map "\C-\M-\p" 'anything-previous-source)
(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-page)
(define-key anything-map "\M-v" 'anything-previous-page)
(define-key anything-map "\C-i" 'anything-select-action)
(define-key anything-map "\C-s" 'anything-isearch)

;; anythingを、ソースを限定して実行する関数を作成するマクロ。
;; 使いかたは、kill-ringのとこを参照。
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources#toc16
(defun something (&rest sources)
  (let ((anything-sources sources))
    (call-interactively 'anything)))

(defmacro something-command (&rest sources)
  `(lambda ()
     (interactive)
     (something ,@sources)))

;; [2008-03-12]
;; 書いてみたけど、まともに動かない。

;; anything-cpan-modules
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources#toc15
(defvar anything-cpan-modules-search-url
  "http://search.cpan.org/search?mode=modules&n=100&query=")

(defun anything-cpan-modules-fetch (input)
  (let ((result (with-current-buffer
                    (url-retrieve-synchronously
                     (concat anything-cpan-modules-search-url
                             (url-hexify-string input)))
                  (buffer-substring (point-min) (point-max))))
        (start 0)
        (modules ()))
    (while (string-match "<a href=\"/author/\\([^\"]+\\)\"><b>\\(.+\\)</b></a>" result start)
      (push (cons (match-string 2 result)
                  (match-string 1 result))
            modules)
      (setq start (match-end 1)))
    (nreverse modules)))

(defun anything-cpan-modules-show-pod (candidate)
  (require 'cperl-mode)
  (let ((result (with-current-buffer
                    (url-retrieve-synchronously (concat "http://search.cpan.org/src/"
                                                        candidate))
                  (buffer-substring (point-min) (point-max))))
        (temp-file (concat "/tmp/cpan-modules/" candidate))
        (auto-mode-alist nil))
    (with-current-buffer (set-buffer (find-file-noselect temp-file nil))
      (erase-buffer)
      (insert result)
      (save-buffer)
      (cperl-pod-to-manpage))))

(defvar anything-c-source-cpan-modules
  '((name . "CPAN Modules")
    (candidates . (lambda ()
                    (anything-cpan-modules-fetch anything-input)))
    (action . (("Show Pod" . anything-cpan-modules-show-pod)))
    (requires-pattern . 3)
    (delayed)))

(global-set-key "\C-c\C-p\C-p" (something-command anything-c-source-cpan-modules))

;; [2008-03-12]
;; multilineパッチが当たらなかったので断念。

;; anything-kill-ring
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources#toc16
;; (defvar anything-kill-ring-threshold 10)
;; (defvar anything-c-source-kill-ring
;;   '((name . "Kill Ring")
;;     (init . (lambda ()
;;               (setq anything-kill-ring-buffer (current-buffer))))
;;     (candidates . (lambda ()
;;                     (remove-if
;;                      (lambda (kill)
;;                        (or (< (length kill) anything-kill-ring-threshold)
;;                            (string-match "^[\\s\\t]+$" kill)))
;;                      kill-ring)))
;;     (action . (("Insert" . (lambda (candidate)
;;                              (with-current-buffer anything-kill-ring-buffer
;;                                (insert candidate))))))
;;                                         ;(requires-pattern . 3)
;;     (multiline)))
;;
;; (global-set-key "\M-y" (something-command anything-c-source-kill-ring))

;; anything-dabbrev-expand
;; http://d.hatena.ne.jp/rubikitch/20080114/anythingdabbrev
(require 'anything-dabbrev-expand)
(global-set-key "\C-u" 'anything-dabbrev-expand)
(define-key anything-dabbrev-map "\C-u" 'anything-dabbrev-find-all-buffers)

;; occur in anything
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources#toc11
(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

;; extended command history
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources#toc19
(defvar anything-c-source-extended-command-history
  '((name . "Emacs Commands History")
    (candidates . extended-command-history)
    (type . command)))

;; sources
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-file-name-history
            anything-c-source-files-in-current-dir
            anything-c-source-locate
            anything-c-source-man-pages
            anything-c-source-info-pages
            anything-c-source-occur
            anything-c-source-extended-command-history
            anything-c-source-emacs-commands))

;; アクションを拡張して別ウィンドウで開けるようにする
;; http://www.emacswiki.org/cgi-bin/emacs/download/RubikitchAnythingConfiguration
(defun anything-c-action-replace (source new-action)
  (setf (cdr (assq 'action (symbol-value source))) new-action)
  (symbol-value source))

;; [2008-03-13]
;; なんか思い通りにならないのでちょといじったけど、アレだなあ。

(defun anything-c-action-extend (description function)
  `((,description . ,function)
    (,(concat description " (other window)")
     . (lambda (c)
         (save-excursion
           (if (one-window-p)
               (select-window (split-window))
             (other-window 1))
           (,function c)
           (other-window -1))))))

;; womanを別ウィンドウで開くアクションを追加
(anything-c-action-replace
 'anything-c-source-man-pages
 (anything-c-action-extend "Woman opens other window" 'woman))
