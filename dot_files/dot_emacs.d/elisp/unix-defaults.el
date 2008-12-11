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

;; UTF-8の優先順位を高くする
(prefer-coding-system 'utf-8-unix)

(if window-system
  (progn
;; GUIでsystem-typeがGNU/Linuxの場合は
;; Bitstream Vera Sans Mono/VLゴシックを指定
;; (要:ttf-bitstream-veraパッケージ)
    (cond
      ((eq system-type 'gnu/linux)
        (set-default-font "Bitstream Vera Sans Mono-8")
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("VL ゴシック" . "unicode-bmp"))
      ))
;;フレーム設定
    (setq default-frame-alist
          (append (list '(top . 0) ; 起動時の表示位置（上から）
                        '(left . 0) ; 起動時の表示位置（左から）
                        '(width . 80) ; 起動時のサイズ（幅）
                        '(height . 40) ; 起動時のサイズ（縦）
                        '(foreground-color . "#00FF00") ; 文字の色
                        '(background-color . "#000000") ; 背景の色
                        '(border-color . "#000000") ;
                        '(mouse-color . "#00FFFF") ;
                        '(cursor-color . "#FF0000") ; カーソルの色
                        '(vertical-scroll-bars . nil) ;
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
