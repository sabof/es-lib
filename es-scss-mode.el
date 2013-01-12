(require 'es-css-mode)

(defun es-scss-comment-line-p ()
  (interactive)
  (cond ( (save-excursion
            (back-to-indentation)
            (or (looking-at "/\\*")
                (looking-at "//")))
          1 )
        ( (nth 4 (syntax-ppss))
          t)))

(defvar es-scss-mode-syntax-table
  (let ((st (make-syntax-table css-mode-syntax-table)))
    ;; The following taken from https://github.com/antonj/scss-mode
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(define-derived-mode es-scss-mode es-css-mode
  "ES SCSS Mode " "Mode for scss"
  (set (make-local-variable 'es-css-comment-line-p-function)
       'es-scss-comment-line-p))

(provide 'es-scss-mode)