
(defun es-css-comment-line-p ()
  (interactive)
  (cond ( (save-excursion
            (back-to-indentation)
            (looking-at "/\\*"))
          1 )
        ( (nth 4 (syntax-ppss))
          t)))

(defun es-css-goto-prev-struct-line ()
  (while (and (forward-line -1)
              (es-css-comment-line-p))))

(defvar es-css-debug t)

(defun es-css-ppss ()
  )

(defun es-css-indent-comment ()
  (back-to-indentation)
  (let* (psl-indent
         psl-last-char
         psl-first-char
         ( psl
           (save-excursion
             (es-css-goto-prev-struct-line)
             (setq psl-indent (current-indentation))
             (setq psl-last-char (char-before (es-visible-end-of-line)))
             (setq psl-first-char
                   (char-after (es-indentation-end-pos)))
             (buffer-substring
              (line-beginning-position)
              (line-end-position))))
         ( psl-closing-brackets
           (+ (count ?} psl)
              (count ?\) psl)))
         ( psl-open-brackets
           (+ (count ?{ psl)
              (count ?\( psl)))
         ( psl-has-colon (plusp (count ?: psl)))
         (base psl-indent)
         (ppss (syntax-ppss))
         previous-comment-indent
         previous-line-was-comment
         this-line-comment-type)
    (cond (;; If "outside" always indent to 0
           (zerop (nth 0 ppss))
           (indent-line-to 0))
          (;; If the previous line is a comment, indent likewise
           (and (setq this-line-comment-type (es-css-comment-line-p))
                (save-excursion
                  (es-goto-previous-non-blank-line)
                  (setq previous-line-was-comment (es-css-comment-line-p)
                        previous-comment-indent (current-indentation))))
           (c-indent-line)
           ;; (cond ( (= 1 this-line-comment-type)
           ;;         (indent-line-to previous-comment-indent))
           ;;       ( t ))
           ;; (cond ( (and (progn (back-to-indentation)
           ;;                     (looking-at "\\*/"))
           ;;              (eq t previous-line-was-comment))
           ;;         (indent-line-to (- previous-comment-indent 3)))
           ;;       ( (eq this-line-comment-type previous-line-was-comment)
           ;;         (indent-line-to previous-comment-indent))
           ;;       ( (eq 1 previous-line-was-comment )
           ;;         (indent-line-to (+ 3 previous-comment-indent)))
           ;;       ( (eq t previous-line-was-comment )
           ;;         (indent-line-to (- previous-comment-indent 3)))
           ;;       ( t (indent-line-to previous-comment-indent)))
           )
          (;; End of colon block
           (and (not psl-has-colon)
                (equal psl-last-char ?\;)
                (zerop psl-open-brackets)
                (zerop psl-closing-brackets)
                (not (member (char-after (es-indentation-end-pos))
                             '( ?\} ?\) ) )))
           (when es-css-debug
             (message "EOC o:%s c:%s"
                      psl-open-brackets psl-closing-brackets))
           (indent-line-to (max 0 (- psl-indent css-indent-offset))))
          (;; Not-first member of comma ending lines
           (and (not (search ": " psl))
                (equal psl-last-char ?\, )
                (zerop psl-open-brackets)
                (zerop psl-closing-brackets))
           (when es-css-debug
             (message "MCB o:%s c:%s"
                      psl-open-brackets psl-closing-brackets))
           (indent-line-to psl-indent))
          (;; Line after beginning of comma block
           (and (member psl-last-char '( ?: ?\, ) )
                ;; Hack
                ;; (search ": " psl)
                ;; (not (equal psl-last-char ?\;))
                (every 'zerop
                       (list psl-open-brackets
                             psl-closing-brackets)))
           (when es-css-debug
             (message "LABOC <%s>"
                      (char-to-string psl-last-char)))
           (indent-line-to (+ psl-indent css-indent-offset)))
          ( t
            (let ((num (* (- (nth 0 ppss)
                             (if (member (char-after (es-indentation-end-pos))
                                         '( ?\} ?\) ) )
                                 1 0))
                          css-indent-offset)))
              (indent-line-to num)
              (message "LAST b:%s e:%s c:%s sc:%s"
                       num
                       (equal (char-after (es-indentation-end-pos)) ?\} )
                       psl-closing-brackets
                       (char-to-string psl-first-char))))
          ( nil
            (when (member psl-first-char '( ?} ?\) ) )
              (incf base css-indent-offset))
            (message "LAST b:%s o:%s fc:%s"
                     base psl-open-brackets psl-first-char)
            (incf base (* psl-open-brackets css-indent-offset))
            (decf base (* psl-closing-brackets css-indent-offset))
            (indent-line-to (max 0 base))))))

(defun* es-css-indent-line ()
  (interactive)
  (save-excursion
    (es-css-indent-comment))
  (when (< (current-column) (current-indentation))
    (back-to-indentation))
  (return-from es-css-indent-line)
  (save-excursion
    (es-css-indent-comment)

    (cond ( (= (line-number-at-pos) 1)
            (indent-line-to 0))
          ( (es-css-comment-line-p)
            (es-css-indent-comment))
          ( (save-excursion
              (es-goto-previous-non-blank-line)
              (and (es-line-matches-p ":[ ]\\|:$")
                   (not (es-line-matches-p "[;({]"))))
            (add-indentation css-indent-offset))))
  )

(defun* css-beginning-of-defun (&optional (arg 1))
  (when (progn
          (unless (zerop (current-column))
            (end-of-line))
          (re-search-backward "^[^\n 	].+{[ ]?$" nil t arg))
    (while (save-excursion
             (and (zerop (forward-line -1))
                  (es-line-matches-p "^[^}[:space:]/]")))
      (forward-line -1))))

(defun es-css-inside-block ()
  (plusp (nth 0 (syntax-ppss))))

(defun* css-end-of-defun (&optional (arg 1))
  (interactive)
  (ignore-errors
    (if (es-css-inside-block)
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