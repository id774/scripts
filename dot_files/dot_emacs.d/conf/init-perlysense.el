;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; perly-sense
;; http://search.cpan.org/dist/Devel-PerlySense/

;; C-C C-p C-o -- Overview -- Show information about the Class at point or
;; the current Class.

;; C-C C-p C-d -- Docs -- Show docs (POD/signature/etc) for the symbol
;; (module/method/sub) at point. A doc hint is displayed in the
;; message area (for methods and subs), or a new POD buffer is created
;; (for modules).

;; C-C C-p C-g -- Go To -- Open file at proper location for module,
;; method/sub declaration for the symbol (module/method/sub) at
;; point. If no sub declaration is available (like for generated
;; getters/setters), any appropriate POD is used instead.

;; C-C C-p C-r -- Run file -- Run the current file using the Compilation
;; mode and the settings appropriate for the source type (Test,
;; Module, etc.). Highlight errors and jump to source with C-c C-c.

;; C-C C-p m f -- Perl Module open File -- Open the source file of the
;; module at point.

(setq perly-sense-key-prefix "\C-c\C-p")
(setq perly-sense-load-flymake t)

;; load perly-sense
(setq perly-sense-external-dir (shell-command-to-string "perly_sense external_dir"))
(if (string-match "Devel.PerlySense.external" perly-sense-external-dir)
    (progn
      (message
       "PerlySense elisp files  at (%s) according to perly_sense, loading..."
       perly-sense-external-dir)
      (add-to-load-path (format "%s/%s" perly-sense-external-dir "emacs"))
      (load "perly-sense")
      (if perly-sense-load-flymake (load "perly-sense-flymake"))
      )
  (message "Could not identify PerlySense install dir.
    Is Devel::PerlySense installed properly?
    Does 'perly_sense external_dir' give you a proper directory? (%s)" perly-sense-external-dir)
  )

;; perly-sense-flymake
(set-face-background 'flymake-errline "VioletRed")
(set-face-background 'flymake-warnline "goldenrod")

;; auto-save-buffersの保存時メッセージに、flymakeの表示が消されてしまう
;; のをどうにかしたい。
;; (setq flymake-no-changes-timeout 9999)
;; (setq flymake-start-syntax-check-on-newline nil)
