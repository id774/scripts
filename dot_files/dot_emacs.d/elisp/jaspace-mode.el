;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'text-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))
                                 (t (:foreground "purple"
                                     :background "unspecified"
                                     :strike-through nil
                                     :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))
                                 (t (:foreground "purple"
                                     :background "unspecified"
                                     :strike-through nil
                                     :underline t))))))))

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
