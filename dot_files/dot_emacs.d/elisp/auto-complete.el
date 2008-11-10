;;; auto-complete.el --- Auto completion with popup menu

;; Copyright (C) 2008  MATSUYAMA Tomohiro

;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension provides a way to select a completion with
;; popup menu.
;; I guarantee a quality only on GNU Emacs 22 or higher.

;; To use this extension, locate this file to load-path directory,
;; and add the following code to your .emacs.
;; ------------------------------
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; ------------------------------

;; After installation, you can try this:
;;
;; 1. Switch to emacs-lisp-mode buffer such as .emacs
;; 2. Goto anywhere
;; 3. Type "def"
;; 4. You may see a pop menu after the cursor like:
;;    def-!-
;;    +-----------------+
;;    |defun            |    <- highlight
;;    |defvar           |
;;    |defmacro         |
;;    |       ...       |
;;    +-----------------+
;; 5. You can complete by seleting the menu item
;;    by pressing C-n/<down>, C-p/<up>, and C-m/RET.
;;

;; This extension is so simple that you can extend
;; how Emacs find a target and how Emacs enumerate
;; candidates.
;; I don't have intention to implement heavy functions :-)
;; Enjoy!

;;; History

;; 2008-11-09  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * auto-complete.el: - auto-complete.el-0.0.1 release
;;                          - fix double-width character displaying problem
;;                          - fix menu position following tab character
;;                          - make candidates visible when you are end of line

;;; Code:



(defgroup auto-complete nil
  "Auto completion with popup menu"
  :group 'convenience
  :prefix "auto-complete-")

(defcustom ac-candidate-width 25
  "Max width of candidates."
  :type 'number
  :group 'auto-complete)

(defcustom ac-candidate-max 10
  "Max number of candidates."
  :type 'number
  :group 'auto-complete)

(defcustom ac-modes
  '(emacs-lisp-mode lisp-interaction-mode
                    c-mode c++-mode java-mode
                    perl-mode cperl-mode python-mode
                    makefile-mode sh-mode fortran-mode f90-mode ada-mode
                    xml-mode sgml-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(list symbol)
  :group 'auto-complete)

(defface ac-selection-face
  '((t (:background "blue" :foreground "white")))
  "Face for the selected candidate."
  :group 'auto-complete)

(defface ac-menu-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for candidate menu."
  :group 'auto-complete)

(defvar ac-mode-hook nil
  "Hook for `auto-complete-mode'.")

(defvar ac-menu nil
  "Menu instance.")

(defvar ac-completing nil
  "Non-nil means `auto-complete-mode' is now working on completion.")

(defvar ac-point nil
  "Start point of target.")

(defvar ac-target nil
  "Target string.")

(defvar ac-candidates nil
  "Current candidates.")

(defvar ac-selection nil
  "Current candidate index.")

(defvar ac-find-target-function 'ac-default-find-target
  "When `auto-complete-mode' finds it can start completion
or update candidates, it will call this function to find a
start point of the completion target.

If this function returns an integer, `auto-complete-mode'
will set the substring between the point and current point to `ac-target'.
And also it will start completion or update candidates by using
the `ac-target'.

If this function returns `nil', `auto-complete-mode'
ignore starting completion or stop completing.")

(defvar ac-enum-candidates-function 'ac-default-enum-candidates
  "This function can return candidates as list by
using the `TARGET' that is given as a first argument.")

(defvar ac-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-complete)
    (define-key map "\r" 'ac-done)

    (define-key map "\C-n" 'ac-next)
    (define-key map "\C-p" 'ac-previous)
    
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    map)
  "Keymap for completion.")

(setq minor-mode-map-alist
      (cons (cons 'ac-completing ac-complete-mode-map)
            minor-mode-map-alist))



;;;; Auto completion

(defun ac-setup (point)
  "Setup popup menu."
  (save-excursion
    (goto-char point)
    (let ((column (current-column))
          (line (line-number-at-pos)))
      (forward-line)
      (if (eq line (line-number-at-pos))
          (newline)
        (forward-line -1))
      (setq ac-menu (ac-menu-create (1+ line) column ac-candidate-width ac-candidate-max))
      (setq ac-point point)
      (setq ac-completing t))))

(defun ac-cleanup ()
  "Destroy popup menu."
  (ac-menu-delete ac-menu)
  (setq ac-menu nil)
  (setq ac-completing nil)
  (setq ac-point nil)
  (setq ac-candidates nil)
  (setq ac-selection 0))

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (if ac-candidates
      (ac-select-candidate
       (let ((selection (1+ ac-selection)))
         (if (= selection (length ac-candidates))
             0
           selection)))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (if ac-candidates
      (ac-select-candidate
       (let ((selection (1- ac-selection)))
         (if (< selection 0)
             (1- (length ac-candidates))
           selection)))))

(defun ac-complete-1 ()
  "Try completion."
  (let ((string (overlay-get (ac-menu-line-overlay ac-menu ac-selection) 'real-string)))
    (when string
      (delete-region ac-point (point))
      (insert string)
      (setq ac-target string))))

(defun ac-complete ()
  "Try completion but select next candidate if called twice."
  (interactive)
  (let ((target ac-target))
    (when (string= target (ac-complete-1))
      (ac-next)
      (ac-complete-1))))

(defun ac-done ()
  "Finish completion."
  (interactive)
  (ac-complete-1)
  (ac-abort))

(defun ac-abort ()
  "Abort completion."
  (ac-cleanup))

(defun ac-update-candidates (candidates)
  "Update candidates of popup menu."
  (setq ac-selection 0)
  (setq ac-candidates candidates)
  (setq ac-completing (if candidates t))
  (let ((i 0)
        (height (ac-menu-height ac-menu)))
    ;; show line and set string to the line
    (mapcar
     (lambda (candidate)
       (ac-menu-show-line ac-menu i)
       (ac-menu-set-line-string ac-menu i candidate (if (= i ac-selection) 'ac-selection-face))
       (setq i (1+ i)))
     candidates)
    ;; assure lines visible
    (if (> i (-
              (max 1 (- (window-height)
                        (if mode-line-format 1 0)
                        (if header-line-format 1 0)))
              (1+ (count-lines (window-start) (point)))))
        (recenter (- (1+ i))))
    ;; hide remaining lines
    (while (< i height)
      (ac-menu-hide-line ac-menu i)
      (setq i (1+ i)))))

(defun ac-select-candidate (selection)
  "Select candidate pointed by `SELECTION'."
  (when ac-candidates
    (ac-menu-set-line-string ac-menu ac-selection (nth ac-selection ac-candidates))
    (ac-menu-set-line-string ac-menu selection (nth selection ac-candidates) 'ac-selection-face)
    (setq ac-selection selection)))

(defun ac-default-find-target ()
  "Default implemention for `ac-find-target-function'."
  (require 'thingatpt)
  (car-safe (bounds-of-thing-at-point 'symbol)))

(defun ac-default-enum-candidates (target)
  "Default implemention for `ac-enum-candidates-function'."
  (if (> (length target) 0)
      (let ((i 0)
            candidate
            candidates
            (regexp (concat "\\b" (regexp-quote target) "\\(\\s_\\|\\sw\\)*\\b")))
        (save-excursion
          ;; search backward
          (goto-char ac-point)
          (while (and (< i ac-candidate-max)
                      (re-search-backward regexp nil t))
            (setq candidate (match-string 0))
            (unless (member candidate candidates)
              (setq candidates (cons candidate candidates))
              (setq i (1+ i))))
          ;; search backward
          (goto-char (+ ac-point (length target)))
          (while (and (< i ac-candidate-max)
                      (re-search-forward regexp nil t))
            (setq candidate (match-string 0))
            (unless (member candidate candidates)
              (setq candidates (cons candidate candidates))
              (setq i (1+ i))))
          (nreverse candidates)))))

(defun ac-on-command ()
  "Handle commands."
  (if (or (eq this-command 'self-insert-command)
          (and ac-menu
               (memq this-command
                     '(delete-backward-char
                       backward-delete-char-untabify))))
      (let ((point (funcall ac-find-target-function)))
        (if (or (null point)
                (and ac-menu
                     (/= point ac-point)))
            (ac-abort)
          (if (null ac-menu)
              (ac-setup point))
          (setq ac-target (buffer-substring point (point)))
          (ac-update-candidates
           (funcall ac-enum-candidates-function ac-target))))
    (if (or (not (symbolp this-command))
            (not (string-match "^ac-" (symbol-name this-command))))
        (ac-abort))))

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ac-modes))
      (auto-complete-mode 1)))

(require 'easy-mmode)

(define-minor-mode auto-complete-mode
  "AutoComplete mode"
  :lighter " AC"
  :group 'auto-complete
  (if auto-complete-mode
      (progn
        (make-local-variable 'ac-find-target-function)
        (make-local-variable 'ac-enum-candidates-function)
        (add-hook 'post-command-hook 'ac-on-command nil t)
        (run-hooks 'ac-mode-hook))
    (remove-hook 'post-command-hook 'ac-on-command t)))

(if (fboundp 'define-global-minor-mode)
    (define-global-minor-mode global-auto-complete-mode
      auto-complete-mode auto-complete-mode-maybe
      :group 'auto-complete))



;;;; Popup menu

(defun ac-menu-width (menu)
  "Popup menu width of `MENU'."
  (nth 0 menu))

(defun ac-menu-height (menu)
  "Popup menu height of `MENU'."
  (nth 1 menu))

(defun ac-menu-overlays (menu)
  "Overlays that `MENU' contains."
  (nth 2 menu))

(defun ac-menu-line-overlay (menu line)
  "Return a overlay of `MENU' at `LINE'."
  (aref (ac-menu-overlays menu) line))

(defun ac-menu-hide-line (menu line)
  "Hide `LINE' in `MENU'."
  (let ((overlay (ac-menu-line-overlay menu line)))
    (overlay-put overlay 'invisible nil)
    (overlay-put overlay 'after-string nil)))

(defun ac-menu-show-line (menu line)
  "Show `LINE' in `MENU'."
  (let ((overlay (ac-menu-line-overlay menu line)))
    (overlay-put overlay 'invisible t)))

(defun ac-menu-set-line-string (menu line string &optional face)
  "Set contents of `LINE' in `MENU'."
  (let ((overlay (ac-menu-line-overlay menu line)))
    (overlay-put overlay 'real-string string)
    (funcall (overlay-get overlay 'set-string-function) overlay (ac-menu-trim-line-string menu string) face)))

(defun ac-menu-trim-line-string (menu string)
  "Adjust `STRING' into `MENU'."
  (let ((length 0)
        (width 0)
        (menu-width (ac-menu-width menu))
        (chars (append string nil)))
    (while (progn
             (setq width (+ width (char-width (car chars))))
             (< width menu-width))
      (setq length (1+ length)))
    (if (< length (length string))
        (substring string 0 (if (<= width menu-width)
                                (1+ length)
                              length))
      string)))

(defun ac-menu-hide (menu)
  "Hide `MENU'."
  (dotimes (i (ac-menu-height menu))
    (ac-menu-hide-line menu i)))

(defun ac-menu-last-line-of-buffer ()
  (save-excursion
    (not (eq (forward-line) 0))))

(defun ac-menu-create (line column width height)
  "Create popup menu."
  (save-excursion
    (let ((overlays (make-vector height nil)))
      (goto-line line)
      (dotimes (i height)
        (move-to-column column)
        (let (overlay begin w current-column prefix postfix)
          (setq current-column (current-column))
          (cond
           ((> current-column column)
            (backward-char)
            (setq current-column (current-column))
            (if (> column current-column)
                (setq prefix (make-string (- column current-column) ? ))))
           ((< current-column column)
            (setq prefix (make-string (- column current-column) ? ))))
          (or prefix (setq prefix ""))

          (setq begin (point))
          (setq w (+ width (length prefix)))
          (while (and (not (eolp))
                      (> w 0))
            (setq w (- w (char-width (char-after))))
            (forward-char))
          (if (< w 0)
              (setq postfix (make-string (- w) ? )))
          (if (ac-menu-last-line-of-buffer)
              (setq postfix (concat postfix "\n")))
          
          (setq overlay (make-overlay begin (point)))
          (overlay-put overlay 'prefix prefix)
          (overlay-put overlay 'postfix postfix)
          (overlay-put overlay 'width width)
          (overlay-put overlay 'set-string-function
                       (lambda (overlay string &optional face)
                         (overlay-put overlay
                                      'after-string
                                      (concat (overlay-get overlay 'prefix)
                                              (propertize
                                               (concat string
                                                       (make-string
                                                        (- (overlay-get overlay 'width) (string-width string))
                                                        ? ))
                                               'face (or face 'ac-menu-face))
                                              (overlay-get overlay 'postfix)))))
          (aset overlays i overlay))
        (forward-line))
      (let ((i 100))
        (mapcar (lambda (overlay)
                  (overlay-put overlay 'priority i)
                  (setq i (1+ i)))
                (nreverse (append overlays nil))))
      (list width height overlays))))

(defun ac-menu-delete (menu)
  "Delete `MENU'."
  (mapcar 'delete-overlay (ac-menu-overlays menu)))

(provide 'auto-complete)
;;; auto-complete.el ends here
