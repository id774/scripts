;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(add-to-load-path "~/dev/taskpaper/trunk"
                  "~/fuse/maimai/dev/taskpaper/trunk")

(require 'taskpaper)

(setq taskpaper-dir (expand-file-name "~/.taskpaper/"))

(defun taskpaper ()
  (interactive)
  (let ((filename (concat taskpaper-dir
                          (format-time-string "%Y-%m-%d.taskpaper"))))
    (find-file-other-frame filename)))
