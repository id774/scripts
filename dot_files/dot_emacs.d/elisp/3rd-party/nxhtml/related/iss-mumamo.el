;;; iss-mumamo.el --- Defines multi major mode for Inno Setup files
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-09T23:29:02+0200 Sat
;; Version: 0.2
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'pascal nil t)
(require 'iss-mode nil t)
(require 'mumamo)

(defun mumamo-chunk-iss-code (pos min max)
  "Find [code]...[, return range and `perl-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-quick-static-chunk pos min max "[code]" "{*** End of CODE **}" nil 'pascal-mode nil))

;;;###autoload
(define-mumamo-multi-major-mode iss-mumamo
  "Turn on multiple major modes Inno Setup .iss files.
The code section will be in `pascal-mode' while the rest will be
in `iss-mode'.

\[code]

... this will be in `pascal-mode'. Note the end mark below!

{*** End of CODE **}
"
    ("Inno ISS Family" iss-mode
     (mumamo-chunk-iss-code
      )))

(add-to-list 'auto-mode-alist '("\\.iss\\'" . iss-mumamo))

(provide 'iss-mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iss-mumamo.el ends here
