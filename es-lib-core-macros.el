;;; es-lib-core-macros.el --- Random macros
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
(require 'cl)

(defmacro es-silence-messages (&rest body)
  `(flet ((message (&rest ignore)))
     ,@body))

(defmacro es-while-point-moving (&rest rest)
  (let ((old-point (gensym)))
    `(let (,old-point)
       (while (not (equal (point) ,old-point))
         (setq ,old-point (point))
         ,@rest))))

(defmacro es-neither (&rest args)
  `(not (or ,@args)))

(defmacro es-define-buffer-local-vars (&rest list)
  "Syntax example:
\(es-define-buffer-local-vars
 mvi-current-image-file nil\)"
  (let (result)
    (while list
      (let ((name (pop list))
            (value (pop list)))
        (push `(defvar ,name ,value) result)
        (push `(make-variable-buffer-local (quote ,name)) result)))
    (cons 'progn (nreverse result))))

(defmacro es-back-pop (symbol)
  (let ( (result (gensym)))
    `(let ( (,result (first (last ,symbol))))
       (setq ,symbol (butlast ,symbol))
       ,result)))

(defmacro es-back-push (what where)
  `(setq ,where (append ,where (list ,what))))

(provide 'es-lib-core-macros)
;; es-lib-core-macros.el ends here