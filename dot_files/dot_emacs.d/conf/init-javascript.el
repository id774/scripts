;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; javascript.el
;; http://web.comhem.se/~u83406637/emacs/javascript.el
(add-to-list 'auto-mode-alist '("\\.\\(js\\|as\\|json\\|jsn\\)\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
(setq javascript-indent-level 4)
(setq javascript-auto-indent-flag nil)

;; [2008-03-12]
;; bracket.elにinsert-single-quotationを追加したのに伴い、以下の設定も追従

;; 対応する括弧を自動挿入する。
;; http://d.hatena.ne.jp/khiker/20061119/1163934208
(add-hook 'javascript-mode-hook
          '(lambda()
             (progn
               ;; { で{}を書く
               (define-key javascript-mode-map "{" 'insert-braces)
               ;; ( で()を書く
               (define-key javascript-mode-map "(" 'insert-parens)
               ;; " で""を書く
               (define-key javascript-mode-map "\"" 'insert-double-quotation)
               ;; ' で''を書く
               (define-key javascript-mode-map "'" 'insert-single-quotation)
               ;; [ で[]を書く
               (define-key javascript-mode-map "[" 'insert-brackets)
               ;; Ctrl+c }でregionを{}で囲む
               (define-key javascript-mode-map "\C-c}" 'insert-braces-region)
               ;; Ctrl+c )でregionを()で囲む
               (define-key javascript-mode-map "\C-c)" 'insert-parens-region)
               ;; Ctrl+c ]でregionを[]で囲む
               (define-key javascript-mode-map "\C-c]" 'insert-brackets-region)
               ;; Ctrl+c "でregionを""で囲む
               (define-key javascript-mode-map "\C-c\"" 'insert-double-quotation-region)
               )))
