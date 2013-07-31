;;; es-lib-duplicate.el --- Duplicate current line or region, part of es-lib
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-lib
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'es-lib-core-macros)
(require 'es-lib-total-line)

(defun es-duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let* (( electric-indent-mode)
         ( pnt (point))
         ( start (es-total-line-beginning-position))
         ( end (es-total-line-end-position))
         ( copy-store (buffer-substring start end)))
    (goto-char end)
    (newline)
    (insert copy-store)
    (goto-char (+ 1 end (- pnt start)))))

(defun es-duplicate-region ()
  "Duplicate the active region."
  (interactive)
  (let (( copy-store
          (or (es-active-region-string)
              (error "No active region")))
        b-pos er-pos)
    (goto-char (region-end))
    ;; For things like (mark-paragraph)
    (unless (zerop (current-column))
      (newline))
    (set-mark (setq b-pos (point)))
    (insert copy-store)
    (setq er-pos (- (line-end-position) (point)))
    (unless (or (eq major-mode 'haskell-mode)
                (memq indent-line-function
                      '(insert-tab
                        indent-relative)))
      (indent-region b-pos (point)))
    (goto-char (- (line-end-position) er-pos))
    (activate-mark)
    (setq deactivate-mark nil
          cua--explicit-region-start nil)))

;;;###autoload
(defun es-duplicate-line-or-region ()
  (interactive)
  (if (region-active-p)
      (es-duplicate-region)
      (es-duplicate-line)))

(provide 'es-lib-duplicate)
;; es-lib-duplicate.el ends here
