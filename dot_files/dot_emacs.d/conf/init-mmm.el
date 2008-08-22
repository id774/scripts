;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; mmm-mode
;; HTML内のJavaScriptを書くとき等に、複数のメジャーモードを使用可能にする。
(add-to-load-path "~/.emacs.d/elisp/mmm-mode")

(require 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(set-face-background 'mmm-output-submode-face  "LightBlue")
(set-face-background 'mmm-code-submode-face    "LightGray")
(set-face-background 'mmm-comment-submode-face "LightYellow")
(set-face-background 'mmm-special-submode-face "Yellow")

(mmm-add-classes
 '((php-output
    :submode php-mode
    :front "<\\?php *echo "
    :back "\\?>"
    :include-front t
    :front-offset 5
    :insert ((?e php-echo nil @ "<?php" @ " echo " _ " " @ "?>" @))
    )))

(mmm-add-classes
 '((php-code
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>"
    :insert ((?p php-section nil @ "<?php" @ " " _ " " @ "?>" @)
             (?b php-block nil @ "<?php" @ "\n" _ "\n" @ "?>" @))
    )))

(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )))

(mmm-add-classes
 '((html-script
    :submode javascript-mode
    :delimiter-mode nil
    :front "<script\[^>\]*\\(language=\"javascript\\([0-9.]*\\)\"\\|type=\"text/javascript\"\\)\[^>\]*>"
    :back"</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )))

(global-set-key [F8] 'mmm-parse-buffer)
(add-hook 'html-mode-hook
          '(lambda ()
             (setq mmm-classes '(php-code erb-code html-script html-js embedded-css))
             (mmm-mode-on)))

;; rhtmlの時は、html-helper-modeの代りにhtml-modeを使う。
;; emacs-railsでスニペットを利用するには、html-modeじゃないとダメらしい。
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
