(defun es-css-comment-line-p ()
  (interactive)
  (or (save-excursion
        (back-to-indentation)
        (looking-at "/\\*"))
      (memq (face-at-point)
            '(font-lock-comment-face
              font-lock-comment-delimiter-face))))

(defun es-css-indent-line ()
  (interactive)
  (css-indent-line)
  (save-excursion
    (let (previous-comment-indent)
      (cond ( (= (line-number-at-pos) 1)
              (indent-line-to 0))
            ( (save-excursion
                (es-goto-previous-non-blank-line)
                (and (es-line-matches-p ":[ ]\\|:$")
                     (not (es-css-comment-line-p))
                     (not (es-line-matches-p "[;({]"))))
              (add-indentation css-indent-offset))
            ( (and (es-css-comment-line-p)
                   (save-excursion
                     (es-goto-previous-non-blank-line)
                     (when (es-css-comment-line-p)
                       (setq previous-comment-indent
                             (current-indentation)))))
              (indent-line-to previous-comment-indent))
            ( (and (es-css-comment-line-p)
                   (not (eq (save-excursion
                              (es-goto-previous-non-blank-line)
                              (last-character))
                            ?\} )))
              (indent-line-to css-indent-offset)))))
  (when (< (current-column) (current-indentation))
    (back-to-indentation)))

(defun* css-beginning-of-defun (&optional (arg 1))
  (when (re-search-backward "^[^\n 	].+{[ ]?$" nil t arg)
    (while (save-excursion
             (forward-line -1)
             (es-line-matches-p "^[^}[:space:]/]"))
      (forward-line -1))))

(defun* css-end-of-defun (&optional (arg 1))
  (interactive)
  (when (re-search-forward "}" nil t arg)
    (ignore-errors
      (forward-char))
    t))

(defun es-hacks-css-mode-setup ()
  (setq indent-line-function 'es-css-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'css-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'css-end-of-defun)
  (set (make-local-variable 'es-aai-indent-function)
       'es-aai-indent-defun))

(provide 'es-hacks-css-mode)