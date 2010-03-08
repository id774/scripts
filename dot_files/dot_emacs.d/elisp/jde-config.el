;; jde-config
(add-hook 'jde-mode-hook
          '(lambda ()
            ))

(let ((elem (assq 'encoded-kbd-mode minor-mode-alist)))
    (when elem
        (setcar (cdr elem) "")))

(c-add-style "java2"
       '((c-basic-offset . 4)
         (setq indent-tabs-mode nil)
         (c-comment-only-line-offset 0 . 0)
         (c-hanging-comment-starter-p)
         (c-offsets-alist .
                  ((inline-open . 0)
                   (topmost-intro-cont . +)
                   (statement-block-intro . +)
                   (knr-argdecl-intro . 5)
                   (substatement-open . +)
                   (label . 0)
                   (statement-case-open . +)
                   (statement-cont . +)
                   (arglist-intro . +)
                   (arglist-close . 0)
                   (access-label . 0)
                   (inher-cont . c-lineup-java-inher)
                   (func-decl-cont . c-lineup-java-throws)
                   ))))

(setq java-mode-hook
    (function (lambda()
        (c-set-style "java2"))))

;; ecb-config
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)

(defun ecb-toggle ()
  (interactive)
    (if ecb-minor-mode
          (ecb-deactivate)
              (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
