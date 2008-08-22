;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; haskell-mode
;; http://www.haskell.org/haskell-mode/
(add-to-load-path "~/.emacs.d/elisp/haskell-mode/")

(load "haskell-site-file")
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
