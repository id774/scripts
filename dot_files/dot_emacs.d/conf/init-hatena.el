;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(add-to-load-path "~/dev/simple-hatena-mode/trunk"
                  "~/fuse/maimai/dev/simple-hatena-mode/trunk")

(require 'simple-hatena-mode)
(require 'hatenahelper-mode)

(setq simple-hatena-time-offset 6)

(add-hook 'simple-hatena-mode-hook
          '(lambda ()
             (hatenahelper-mode 1)))
(add-hook 'simple-hatena-before-submit-hook
          '(lambda ()))
(add-hook 'simple-hatena-after-submit-hook
          '(lambda ()))
