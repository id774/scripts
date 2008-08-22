;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; grepをackに変更
;; 事前にackをcpanでインストールすること。debianパッケージだと、ack-grep
(setq grep-command "ack -a --nocolor ")
(defun ack ()
  (interactive)
  (let ((grep-find-command "ack --nocolor --nogroup "))
    (call-interactively 'grep-find)))

;; brackets.el
;; 対応する括弧を自動補完。各言語モードで設定
(load "brackets.el")

;; shellっぽいところで、C-n/pでヒストリ
(define-key comint-mode-map "\C-p" 'comint-previous-input)
(define-key comint-mode-map  "\C-n" 'comint-next-input)

;; flymake
;; 自動的にシンタックスチェックをかける
(require 'flymake)
(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; リージョンを自動的に囲む
;; http://sami.samhuri.net/2007/6/23/emacs-for-textmate-junkies
(defun wrap-region (left right beg end)
  "Wrap the region in arbitrary text, LEFT goes to the left and RIGHT goes to the right."
  (interactive)
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)))

(defmacro wrap-region-with-function (left right)
  "Returns a function which, when called, will interactively `wrap-region-or-insert' using LEFT and RIGHT."
  `(lambda () (interactive)
     (wrap-region-or-insert ,left ,right)))

(defun wrap-region-with-tag-or-insert ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'wrap-region-with-tag)
    (insert "<")))

(defun wrap-region-with-tag (tag beg end)
  "Wrap the region in the given HTML/XML tag using `wrap-region'. If any
attributes are specified then they are only included in the opening tag."
  (interactive "*sTag (including attributes): \nr")
  (let* ((elems    (split-string tag " "))
         (tag-name (car elems))
         (right    (concat "</" tag-name ">")))
    (if (= 1 (length elems))
        (wrap-region (concat "<" tag-name ">") right beg end)
      (wrap-region (concat "<" tag ">") right beg end))))

(defun wrap-region-or-insert (left right)
  "Wrap the region with `wrap-region' if an active region is marked, otherwise insert LEFT at point."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (wrap-region left right (region-beginning) (region-end))
    (insert left)))

(global-set-key "'"  (wrap-region-with-function "'" "'"))
(global-set-key "\"" (wrap-region-with-function "\"" "\""))
(global-set-key "`"  (wrap-region-with-function "`" "`"))
(global-set-key "("  (wrap-region-with-function "(" ")"))
(global-set-key "["  (wrap-region-with-function "[" "]"))
(global-set-key "{"  (wrap-region-with-function "{" "}"))
(global-set-key "<"  'wrap-region-with-tag-or-insert)

;; TDD支援
;; http://d.hatena.ne.jp/xcezx/20080305/1204698047
(defvar tdd-color-alist
  '(("Think"       . (lambda ()
                       (set-face-attribute 'mode-line nil
                                           :foreground "brightwhite"
                                           :background "brightblue")))
    ("Red"         . (lambda ()
                       (set-face-attribute 'mode-line nil
                                           :foreground "brightwhite"
                                           :background "brightred")))
    ("Green"       . (lambda ()
                       (set-face-attribute 'mode-line nil
                                           :foreground "brightwhite"
                                           :background "brightgreen")))
    ("Refactoring" . (lambda ()
                       (set-face-attribute 'mode-line nil
                                           :foreground "black"
                                           :background "brightyellow")))))

(defvar tdd-color-mode 3)
(defvar tdd-color-mode-name "")
(defvar tdd-color-mode-foreground nil)
(defvar tdd-color-mode-background nil)
(let ((cell (or (memq 'mode-line-position mode-line-format)
                (memq 'mode-line-buffer-identification mode-line-format)))
      (newcdr 'tdd-color-mode-name))
  (unless (member newcdr mode-line-format)
    (setcdr cell (cons newcdr (cdr cell)))))

(defun tdd-color-rotate ()
  (interactive)
  (let (pair)
    (if (>= tdd-color-mode 3)
        (setq tdd-color-mode 0)
      (setq tdd-color-mode
            (+ tdd-color-mode 1)))
    (setq pair
          (nth tdd-color-mode tdd-color-alist))
    (setq tdd-color-mode-name (format "[%s]" (car pair)))
    (funcall (cdr pair))))

(global-set-key "\C-cm" 'tdd-color-rotate)
