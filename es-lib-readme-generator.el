;;; -*- lexical-binding: t -*-
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
         ( macros (remove-if-not 'apropos-macrop funs-raw))
         ( vars (remove-if-not 'symbolp all-syms)))
    (list :defuns funs
          :macros macros
          :defvars vars
          :aliases aliases)))

(defun es--type-name (type)
  (case type
    (:defuns "Commands")
    (:defvars "Non-interactive")
    (:macros "Macros")
    (:aliases "Aliases")))

(let ((total-items 0))
  (defun es-package-report (feature)
    (es-reload-feature feature)
    (let ((analyzed (es-analyze-feature-loadhist feature)))
      (with-temp-buffer
        (insert "### " (symbol-name feature) "\n")
        (dolist (type '(:defuns :macros :defvars))
          (insert "#### " (es--type-name type) ":\n")
          (dolist (item (getf analyzed type))
            (insert ))))))

  (defun es-lib-index ()
    (flet (( print-symbol-list (list)
             (dolist (sym list)
               (insert "* " (symbol-name sym) "\n")
               (when (documentation sym)
                 (insert "\n```\n")
                 (let* ((parts (split-string (documentation sym) "\n"))
                        (last (last parts)))
                   ;; (setf (first parts) (concat "<em>" (first parts)) )
                   ;; (setf (car last) (concat (car last) "</em>"))
                   (apply 'insert
                          (mapcar
                           (lambda (part)
                             (concat ;; "    "
                              part "\n"))
                           parts)))
                 (insert "```\n\n")
                 ))))
      (save-excursion
        (save-window-excursion
          (let (( libs (mapcar
                        'file-name-base
                        (directory-files
                         (mmake-path "site-lisp/my-scripts/es-lib")
                         nil "^es-")))
                defuns commands non-interactive macros)
            (dolist (lib libs)
              (find-library lib)
              (eval-buffer)
              (setq defuns (append (buffer-defuns) defuns))
              (setq macros (append (buffer-macros) macros)))
            (setq total-items (reduce '+ (list defuns macros)
                                      :key 'length))
            (setq commands (remove-if-not 'commandp defuns))
            (setq non-interactive (remove-if 'commandp defuns))
            (setq commands (cl-sort commands 'string< :key 'symbol-name))
            (setq non-interactive (cl-sort non-interactive 'string< :key 'symbol-name))
            (setq macros (cl-sort macros 'string< :key 'symbol-name))
            (with-temp-buffer
              (insert "\n### Commands:\n\n")
              (print-symbol-list commands)

              (insert "\n### Non-interactive:\n\n")
              (print-symbol-list non-interactive)

              (insert "\n### Macros:\n\n")
              (print-symbol-list macros)
              (buffer-string)))))))

  (defun es-lib-make-readme ()
    (interactive)
    (let ((index (es-lib-index)))
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

")
          ;; Could also insert (documentation)
          (insert (format "## Index:
_Auto-generated before each commit. Total items in the library: %s_
"
                          total-items))
          (insert index)
          (save-buffer))))))

(provide 'es-lib-readme-generator)