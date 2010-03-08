;; http://d.hatena.ne.jp/rubikitch/20081104/1225745862
;; http://d.hatena.ne.jp/yaotti/20081104/1225809687
;; http://gist.github.com/22143

(setq view-read-only t)
(defvar pager-keybind
      `( ;; vi-like
        ("h" . backward-word)
        ("l" . forward-word)
        ("j" . next-line)
        ("k" . previous-line)
        ("J" . next-window-line)
        ("K" . previous-window-line)
        ("b" . scroll-down)
        ("f" . scroll-up)
        (" " . scroll-up)
        ("w" . forward-word)
        ("e" . backward-word)
        ("n" . ,(lambda () (interactive) (scroll-up 1)))
        ("p" . ,(lambda () (interactive) (scroll-down 1)))
        ("[" . forward-sexp)
        ("]" . backward-sexp)
        ("." anything-c-moccur-occur-by-moccur)
        ("c" . scroll-other-window-down)
        ("v" . scroll-other-window)
        ))
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)

(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  (setq global-hl-line-mode t)
  (define-key view-mode-map " " 'scroll-up))
(add-hook 'view-mode-hook 'view-mode-hook0)

;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))
;; 書き込み不能な場合はview-modeを抜けないように
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)

(provide 'view-support)
