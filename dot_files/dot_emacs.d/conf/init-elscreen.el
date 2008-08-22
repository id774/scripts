;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(add-to-load-path "~/.emacs.d/elisp/elscreen")

(setq elscreen-prefix-key "\C-z")
(setq elscreen-buffer-to-nickname-alist
      '(("^mew-" . "Mew")
        ("^riece-" . "Liece")
        ("^dired-mode$" . "Dired")
        ("^Info-mode$" . "Info")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("dict" . "OnlineDict")))
(setq elscreen-display-tab nil)

(load "elscreen")

;; plugins
(load "elscreen-dnd")
(load "elscreen-gf")
(load "elscreen-dired")
(load "elscreen-server")
(load "elscreen-w3m")
