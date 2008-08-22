;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; Gauche用設定
;; http://karetta.jp/book-node/gauche-hacks/004682
;;
;; * M-C space barカーソルの次のＳ式をマーク
;; * M-C-a カーソルを含むトップレベルのＳ式の先頭へ移動
;; * M-C-e カーソルを含むトップレベルのＳ式の末尾へ移動
;; * M-C-f 次のＳ式へ移動
;; * M-C-b 前のＳ式へ移動
;; * M-C-t カーソルの前後のＳ式を交換
;; * M-C-d 1レベル内側のＳ式へ移動
;; * M-C-u 1レベル外側のＳ式へ移動

(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))

;; gca.el
;; http://practical-scheme.net/wiliki/wiliki.cgi?GaucheFest%3akoguro

(require 'gca)

;; モジュールとシンボルの対応関係をキャッシュしておくためのファイルの名前
(setq gca-module-db-filename (expand-file-name "~/.emacs.d/gauche/module-db"))

;; C-c C-uで(use module)をインサートする
(define-key scheme-mode-map "\C-c\C-u" 'gca-insert-use)

(let ((m (make-sparse-keymap)))
  ;; C-c C-d h でドキュメントを検索する
  (define-key m "h" 'gca-show-info)
  ;; C-c C-d i でauto-info-modeの切り替えを行う。(ONの場合自動的にinfoを表示します)
  (define-key m "i" 'auto-info-mode)
  (define-key scheme-mode-map "\C-c\C-d" m))

;; C-c C-,でinfoの次のトピックを表示します(検索結果が複数あった場合)
(define-key scheme-mode-map [(control c) (control ,)] 'gca-info-next)

;; C-. でシンボルを補完する
(define-key scheme-mode-map [(control .)] 'gca-completion-current-word)

;; C-c C-. でコードのひな形をインサートする
(define-key scheme-mode-map [(control c) (control .)] 'gca-insert-template)
(define-key c-mode-map [(control c) (control .)] 'gca-insert-template)

;; C-c C-t でテストケースをインサートする (run-schemeでtgosh.scmを使う必
;; 要があります) C-u で引数を与えると、その番号で実行された結果をもとに
;; してテストケースを作成します。省略時は直前の実行結果が使われます。
(define-key scheme-mode-map "\C-c\C-t" 'gca-make-test)

;; C-c C-h で履歴をみる (run-schemeでtgosh.scmを使う必要があります)
(define-key scheme-mode-map "\C-c\C-h" 'gca-show-history)

;; 単体テストケース作成の支援(gca-make-test, gca-show-history)を使うに
;; はrun-schemeで付属のtgosh.scmが起動するようにしてください。
(setq scheme-program-name "gosh ~/.emacs.d/gauche/tgosh.scm")

;; ウィンドウを2つに分け、一方でgoshインタプリタを実行するコマンド
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

;; C-c rでgoshインタプリタを起動する
(global-set-key "\C-cr" 'scheme-other-window)

;; インデントの定義
(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)

;; ElDocでドキュメント表示
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
  (lambda ()
    (setq eldoc-info-function 'scheme-get-current-symbol-info)
    (eldoc-mode)))
