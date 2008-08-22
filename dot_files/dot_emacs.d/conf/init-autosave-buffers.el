;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(add-to-load-path "~/dev/auto-save-buffers-enhanced/trunk"
                  "~/fuse/maimai/dev/auto-save-buffers-enhanced/trunk")

(require 'auto-save-buffers-enhanced)

(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-exclude-regexps '("\\.hatena"))
(global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)
(global-set-key "\C-xar" 'auto-save-buffers-enhanced-reload-svk)

;; (setq auto-save-buffers-enhanced-use-svk-flag t)
;; (auto-save-buffers-enhanced-include-only-checkout-path t)
