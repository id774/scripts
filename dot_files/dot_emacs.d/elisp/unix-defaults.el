;; Unix default settings

;; 日本語設定
(set-language-environment 'Japanese)
;; 最近もう$LANGでいいやという気になった。
;; (set-default-coding-systems 'euc-jp-unix)
;; (set-buffer-file-coding-system 'euc-jp-unix)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'euc-jp-unix)
;; (setq file-name-coding-system 'euc-jp-unix)
;; (set-clipboard-coding-system 'iso-2022-jp-unix)
;; (setq default-process-coding-system '(undecided . euc-jp-unix))

(if window-system
  (progn
    (set-default-font "Bitstream Vera Sans Mono-10")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("VL ゴシック" . "unicode-bmp"))
;;フレーム設定
    (setq default-frame-alist
          (append (list '(top . 50) ; 起動時の表示位置（上から）
                        '(left . 50) ; 起動時の表示位置（左から）
                        '(width . 150) ; 起動時のサイズ（幅）
                        '(height . 70) ; 起動時のサイズ（縦）
                        '(foreground-color . "#FFFFFF") ; 文字の色
                        '(background-color . "gray30") ; 背景の色
                        '(cursor-color . "gray") ; カーソルの色
                   )
                  default-frame-alist))
;;リージョンに色を付ける
    (setq transient-mark-mode t)
;;フォントロック
    (global-font-lock-mode 1)
    (setq font-lock-support-mode 'jit-lock-mode)
;; 色づけは最大限に
    (setq font-lock-maximum-decoration t)
;; デフォルトの色づけを変える
    (add-hook 'font-lock-mode-hook '(lambda ()
      (set-face-foreground 'font-lock-builtin-face "spring green")
      (set-face-foreground 'font-lock-comment-face "slate gray")
      (set-face-foreground 'font-lock-string-face  "spring green")
      (set-face-foreground 'font-lock-keyword-face "khaki")
      (set-face-foreground 'font-lock-constant-face "violet")
      (set-face-foreground 'font-lock-function-name-face "hot pink")
      (set-face-foreground 'font-lock-variable-name-face "hot pink")
      (set-face-foreground 'font-lock-type-face "cyan")
      (set-face-foreground 'font-lock-warning-face "magenta")
      (set-face-bold-p 'font-lock-function-name-face t)
      (set-face-bold-p 'font-lock-warning-face nil)
    ))
))
;;


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
