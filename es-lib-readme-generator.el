;;; -*- lexical-binding: t -*-
;; For internal use, and to satisfy curiosity.

(require 'loadhist)
(require 'apropos)

(defun es-reload-feature (feature)
  "Not very nice, but ensures that all definitions are fresh."
  (when (featurep feature)
    (ignore-errors
      (unload-feature feature t)))
  (require feature))

(defun es-analyze-feature-loadhist (feature)
  (let* (( all-syms (feature-symbols feature))
         ( funs-raw (mapcar 'cdr
                            (cl-remove-if-not
                             (lambda (thing)
                               (and (consp thing)
                                    (eq (car thing)
                                        'defun)))
                             all-syms)))
         ( aliases (prog1 (cl-remove-if-not
                           'symbolp
                           funs-raw
                           :key 'symbol-function)
                     (setq funs-raw
                           (cl-remove-if-not
                            'consp
                            funs-raw
                            :key 'symbol-function))))
         ( funs (remove-if 'apropos-macrop funs-raw))
         ( commands (prog1 (remove-if-not 'commandp funs)
                      (setq funs (remove-if 'commandp funs))))
         ( macros (remove-if-not 'apropos-macrop funs-raw))
         ( vars (remove-if-not 'symbolp all-syms)))
    (list :defuns-ni funs
          :commands commands
          :macros macros
          :defvars vars
          :aliases aliases)))

(defun es--type-name (type)
  (case type
    (:defuns-ni "Non-interactive")
    (:commands "Commands")
    (:defvars "Defvars")
    (:macros "Macros")
    (:aliases "Aliases")))

(defun es-lib-features ()
  (let ((libs (mapcar (es-comp 'intern 'file-name-base)
                      (directory-files
                       (mmake-path "site-lisp/my-scripts/es-lib")
                       nil "^es-"))))
    (remove-if (lambda (featch)
                 (memq featch
                       '(es-lib-readme-generator
                         es-lib)))
               libs)))

(defun es-toc (features)
  (with-temp-buffer
    (insert "\n")
    (dolist (feature features)
      (insert
       (format "* [%s](#%s)\n"
               feature feature)))
    (insert "\n")
    (buffer-string)))

(let ((total-items 0))
  (defun es-feature-report (feature)
    (es-reload-feature feature)
    (let ((analyzed (es-analyze-feature-loadhist feature)))
      (with-temp-buffer
        (insert "### " (symbol-name feature) "\n")
        (dolist (type '(:defvars :macros :commands :defuns-ni))
          (unless (zerop (length (getf analyzed type)))
            (insert "#### " (es--type-name type) ":\n")
            (dolist (item (cl-sort (getf analyzed type) 'string< :key 'symbol-name))
              (unless (search "--" (symbol-name item))
                (insert "* " (symbol-name item) "\n")
                (cond ( (and (eq type :defvars)
                             (apropos-documentation-property
                              item 'variable-documentation t))
                        (insert "\n```\n"
                                (apropos-documentation-property
                                 item 'variable-documentation t)
                                "\n```\n\n"))
                      ( (documentation item)
                        (insert "\n```\n"
                                (documentation item)
                                "\n```\n\n")))
                (incf total-items)))))
        (buffer-string))))

  (defun es-lib-report ()
    (let* (( libs (es-lib-features)))
      (setq total-items 0)
      (with-temp-buffer
        (dolist (feature libs)
          (insert (es-feature-report feature)))
        (buffer-string))))

  (defun es-lib-make-readme ()
    (interactive)
    (let ((index (es-lib-report)))
      (save-excursion
        (save-window-excursion
          (find-file (mmake-path "site-lisp/my-scripts/es-lib/README.md"))
          (erase-buffer)
          (insert "#es-lib
A collecton of emacs utilities. Here are some highlights:

#### Files:

* **es-lib-move-text.el:**
  Functions for shifting current line or region in four directions
* **es-lib-duplicate.el:**
  Functions duplicating the current region
* **es-lib-total-line.el:**
  Functions for comfortably moving with folded lines
* **es-lib-number-at-point:**
  Functions for manipulating the number at point.
* **es-lib-aa-indent:**
  Automatic automatic indentation. Code gets indented as you type. See
  es-aai-mode docstring for details.

#### Functions:
* **es-ack-replace-symbol:**
  A refactoring tool, with help of which this library was assembled

"
                  (format "## Index:
_Auto-generated before each commit. Total items in the library: %s_
"
                          total-items)
                  "### Table of contents \n"
                  (es-toc (es-lib-features))
                  index)
          (save-buffer))))))

(provide 'es-lib-readme-generator)