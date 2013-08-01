;;; es-lib-number-at-point.el --- Manipulate numbers at point
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

(cl-defun es-number-at-point ()
  (unless (looking-at "[[:digit:]]")
    (cl-return-from es-number-at-point))
  (save-excursion
    (skip-chars-backward "0123456789-")
    (unless (looking-at "-?[[:digit:]]+")
      (cl-return-from es-number-at-point))
    (looking-at "[[:digit:]-]+")
    (list (match-string-no-properties 0)
          (match-beginning 0)
          (match-end 0))))

(cl-defun es--change-number-at-point (&optional decrease)
  (let ((number (es-number-at-point)))
    (if (not number)
        (progn (let (( end-distance (- (line-end-position) (point))))
                 ;; (cl-return-from es--change-number-at-point)
                 (when (re-search-backward "[0-9]" (line-beginning-position) t)
                   (es--change-number-at-point decrease))
                 (goto-char (- (line-end-position) end-distance))))
        (cl-multiple-value-bind
            (num-string beg end)
            number
          (let* ((start-pos (point))
                 (distance-from-end (- end start-pos))
                 (increment (* (expt 10 (1- distance-from-end))
                               (if decrease -1 1)))
                 (result (+ (string-to-number num-string) increment))
                 (result-string (number-to-string
                                 result)))
            (delete-region beg end)
            (insert result-string)
            (goto-char (max beg
                            (+ start-pos
                               (- (length result-string)
                                  (length num-string))))))))))

;;;###autoload
(defun es-increase-number-at-point ()
  "Increases the digit at point.
The increment some power of 10, depending on the positon of the cursor. If there
is no number at point, will try to increment the previous number on the same
line."
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es--change-number-at-point)))

;;;###autoload
(defun es-decrease-number-at-point ()
  "See documentation for `es-increase-number-at-point'."
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es--change-number-at-point t)))

(provide 'es-lib-number-at-point)
;; es-lib-number-at-point.el ends here
