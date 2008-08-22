;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ruby-mode
;; http://pub.cozmixng.org/~the-rwiki/rw-cgi.rb?cmd=view;name=Emacs
(add-to-load-path "~/.emacs.d/elisp/ruby-mode/")

(require 'ruby-mode)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(setq auto-mode-alist
      (append '(
                ("\\.rb$"   . ruby-mode)
                ("Rakefile" . ruby-mode)
                ("\\.rake$" . ruby-mode)
                ("\\.rjs"   . ruby-mode)
                ) auto-mode-alist))

(setq interpreter-mode-alist
      (append '(
                ("ruby" . ruby-mode)
                )
              interpreter-mode-alist))

;; 深いインデントを避ける
(setq ruby-deep-indent-paren-style nil)

;; よくあるコードを、自動挿入する。
(require 'ruby-electric)

;; flymake-modeで補完する対象を追加
(push '(".+\\.rb$"   flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$"  flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rake$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rjs$"  flymake-ruby-init) flymake-allowed-file-name-masks)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (ruby-electric-mode t)
             (abbrev-mode nil)
             (flymake-mode t)))

;; rails
;; http://d.hatena.ne.jp/higepon/20061222/1166774270
(add-to-load-path "~/.emacs.d/elisp/emacs-rails/")

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

(require 'rails)

(setq rails-use-mongrel t)
(define-key rails-minor-mode-map "\C-c\C-p" 'rails-lib:run-primary-switch)
(define-key rails-minor-mode-map "\C-c\C-n" 'rails-lib:run-secondary-switch)

;; emacs -nwだと、キーバインドを変更しないと
;; rails-goto-file-on-current-lineが動かない。
;; http://d.hatena.ne.jp/kabus/20070822/1187806296
(define-key rails-minor-mode-map "\C-cj" 'rails-goto-file-on-current-line)

;; emacs-railsで、C-c C-c C-tした時にtagsを作るファイルの置かれたディレクトリ
;; http://d.hatena.ne.jp/Rommy/20070906/p1
(setq rails-tags-dirs '("app" "lib" "test" "db" "vendor"))
(setq rails-tags-command "ctags -e --Ruby-kinds=-f -o %s --exclude='*.html' -R %s")

;; RSpec用スニペット
;; http://www.achama.com/archives/2007/08/rspecsnippet.html
(require 'snippet)
(add-hook 'rails-minor-mode-hook
          '(lambda()
             (snippet-with-abbrev-table 'local-abbrev-table
                                        ("it" ."it \"$${spec}\" do\n$>$.\nend$>\n")
                                        ("sbt"."should be_true")
                                        ("sbi"."should be_an_instance_of($${klass})")
                                        ("se"."shoud == ")
                                        )
             ))

;; Emacs内からReFeのドキュメントを読む。M-x refeで実行。
;; http://i.loveruby.net/ja/prog/refe.html
(require 'refe)

;; Emacs内でautotest実行。M-x autotestで実行。
;; http://www.emacswiki.org/cgi-bin/emacs/download/autotest.el
(require 'autotest)

;; magic comment
;; Ruby1.9から、ファイルの文字コードを明記する必要がある
;; http://d.hatena.ne.jp/rubikitch/20080307/magiccomment
(defun ruby-insert-magic-comment-if-needed ()
  "バッファのcoding-systemをもとにmagic commentをつける。"
  (when (and (eq major-mode 'ruby-mode)
             (find-multibyte-characters (point-min) (point-max) 1))
    (save-excursion
      (goto-char 1)
      (when (looking-at "^#!")
        (forward-line 1))
      (if (re-search-forward "^#.+coding" (point-at-eol) t)
          (delete-region (point-at-bol) (point-at-eol))
        (open-line 1))
      (let* ((coding-system (symbol-name buffer-file-coding-system))
             (encoding (cond ((string-match "japanese-iso-8bit\\|euc-j" coding-system)
                              "euc-jp")
                             ((string-match "shift.jis\\|sjis\\|cp932" coding-system)
                              "shift_jis")
                             ((string-match "utf-8" coding-system)
                              "utf-8"))))
        (insert (format "# -*- coding: %s -*-" encoding))))))

(add-hook 'before-save-hook 'ruby-insert-magic-comment-if-needed)
