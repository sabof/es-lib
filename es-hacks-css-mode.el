
(defun es-css-comment-line-p ()
  (interactive)
  (or (save-excursion
        (back-to-indentation)
        (looking-at "/\\*"))
      (memq (face-at-point)
            '(font-lock-comment-face
              font-lock-comment-delimiter-face))))

(defun es-css-indent-comment ()
  (let* (pnb-indent
         pnb-last-char
         ( pnb
           (save-excursion
             (es-goto-previous-non-blank-line)
             (setq pnb-indent (current-indentation))
             (setq pnb-last-char
                   (char-before
                    (end-of-visible-line)))
             (buffer-substring
              (line-beginning-position)
              (line-end-position))))
         ( pnb-brackets (count ?} pnb))
         ( pnb-has-colon (plusp (count ?: pnb)))
         (base pnb-indent))
    (cond (;; If the previous line is a comment, indent likewise
           (save-excursion
             (es-goto-previous-non-blank-line)
             (when (es-css-comment-line-p)
               (setq previous-comment-indent
                     (current-indentation))))
           (indent-line-to pnb-indent))

          (;; End of colon block
           (and (not pnb-has-colon)
                (equal pnb-last-char ?\;)
                (zerop pnb-brackets))
           (indent-to (max 0 (- pnb-indent css-indent-offset))))
          (
           (or pnb-has-colon (plusp pnb-brackets))
           (+ pnb-indent (if (not pnb-has-colon) ))
           (not (eq (save-excursion
                      (es-goto-previous-non-blank-line)
                      (last-character))
                    ?\} ))
           (indent-line-to css-indent-offset)))))

(defun* es-css-indent-line ()
  (interactive)
  (css-indent-line)
  (save-excursion

    (cond ( (= (line-number-at-pos) 1)
            (indent-line-to 0))
          ( (es-css-comment-line-p)
            (es-css-indent-comment))
          ( (save-excursion
              (es-goto-previous-non-blank-line)
              (and (es-line-matches-p ":[ ]\\|:$")
                   (not (es-line-matches-p "[;({]"))))
            (add-indentation css-indent-offset))))
  (when (< (current-column) (current-indentation))
    (back-to-indentation)))

(defun* css-beginning-of-defun (&optional (arg 1))
  (when (progn
          (unless (zerop (current-column))
            (end-of-line))
          (re-search-backward "^[^\n 	].+{[ ]?$" nil t arg))
    (while (save-excursion
             (forward-line -1)
             (es-line-matches-p "^[^}[:space:]/]"))
      (forward-line -1))))

(defun es-css-inside-function ()
  (save-excursion
    (ignore-errors
      (backward-up-list)
      t)))

(defun* css-end-of-defun (&optional (arg 1))
  (interactive)
  (ignore-errors
    (if (es-css-inside-function)
        (css-beginning-of-defun))
    (progn
      (re-search-forward "{" nil t arg)
      (backward-char)
      (forward-sexp)
      (ignore-errors
        (forward-char)))
    t))

(defun es-hacks-css-mode-setup ()
  (setq indent-line-function 'es-css-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'css-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'css-end-of-defun)
  (set (make-local-variable 'es-aai-indent-function)
       'es-aai-indent-defun)
  ;; (when (and (buffer-file-name)
  ;;            (string-match-p
  ;;             "\\.scss$"
  ;;             (buffer-file-name)))
  ;;   (set (make-local-variable 'es-aai-indentable-line-p-function)
  ;;        (lambda ()
  ;;          (not (es-line-matches-p "^[[:blank:]]*@")))))
  )

(provide 'es-hacks-css-mode)