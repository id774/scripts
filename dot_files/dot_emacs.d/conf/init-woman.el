;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(setq woman-manpath
      (mapcar 'expand-file-name
              (list "~/local/man"
                    "~/fuse/maimai/local/man"
                    "/opt/local/share/man"
                    "/usr/local/share/man"
                    "/usr/share/man"
                    )))

(setq woman-cache-filename (expand-file-name "~/.womancache"))
(setq woman-cache-level 3)
(setq woman-use-own-frame nil)

(autoload 'woman "woman" "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman" "Find, decode and browse a specific UN*X man-page file." t)
