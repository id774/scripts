;; emacs-w3m
(when (autoload-p 'w3m "w3m" "Interface for w3m on Emacs." 'interactive)
  (autoload 'w3m-find-file "w3m" "Find a local file using emacs-w3m." t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)
  (autoload 'w3m-weather "w3m-weather" "Display a weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report changes of web sites." t)
  (autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t)
  (setq w3m-use-cookies t)
  (setq w3m-cookie-accept-bad-cookies t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (global-set-key "\C-xm" 'browse-url-at-point)
  ;; startup.elのProxy情報を参照
  (if global-proxy-use
      (setq w3m-command-arguments-alist
        '(("^http://\\([^/]*\\.\\)hoge\\.co\\.jp\\(/\\|$\\)" "-no-proxy")
        ;; Use the proxy server to visit any other urls.
        ("" "-o" "http_proxy=http://proxy.hoge.co.jp:8080/"))))
  (define-key global-map "\C-c\C-c\C-l" 'w3m)
  (define-key global-map "\C-c\C-c\ l" 'w3m))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
