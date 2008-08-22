;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 色の設定
;; http://norainu.net/mt/archives/2007/05/emacs_22.html
(set-background-color "black")
(set-foreground-color "white")
(set-face-foreground 'font-lock-comment-face "tan1")
(set-face-foreground 'font-lock-string-face  "firebrick")
(set-face-foreground 'font-lock-keyword-face "SandyBrown")
(set-face-foreground 'font-lock-function-name-face "BlueViolet")
(set-face-foreground 'font-lock-variable-name-face "limegreen")
(set-face-foreground 'font-lock-type-face "skyblue1")
(set-face-foreground 'font-lock-warning-face "yellow")
(set-face-foreground 'font-lock-builtin-face "goldenrod")
(set-face-foreground 'font-lock-constant-face "tomato")
(set-face-background 'highlight "grey")
(set-face-foreground 'highlight "black")
(set-face-background 'region "RoyalBlue4")
(set-face-foreground 'region "white")
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "brightwhite")

;; ウィンドウを複数開いた時、アクティヴでない方のモードラインの色
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "brightblack")

;; ミニバッファのプロンプトの色
(set-face-foreground 'minibuffer-prompt "Green")
