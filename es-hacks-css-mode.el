
(defun es-css-comment-line-p ()
  (interactive)
  (let (( ppss (syntax-ppss)))
    (cond ( (save-excursion
              (back-to-indentation)
              (eq (nth 8 (syntax-ppss))
                  (point)))
            1 )
          ( (nth 4 (syntax-ppss))
            t))))

(defun es-css-comment-line-p ()
  (interactive)
  (cond ( (save-excursion
            (back-to-indentation)
            (looking-at "/\\*"))
          1 )
        ( (nth 4 (syntax-ppss))
          t)))

(defun es-css-goto-prev-struct-line ()
  (while (and (zerop (forward-line -1))
              (es-css-comment-line-p))))

(defvar es-css-debug t)

(defun es-css-calculate-indent ()
  (goto-char (line-beginning-position))
  (with-syntax-table css-navigation-syntax-table
    (let* (psl-indent
           psl-last-char
           psl-first-char
           ( psl
             (save-excursion
               (es-css-goto-prev-struct-line)
               (setq psl-indent (current-indentation))
               (setq psl-last-char (char-before (es-visible-end-of-line)))
               (setq psl-first-char (char-after (es-indentation-end-pos)))
               (buffer-substring
                (line-beginning-position)
                (line-end-position))))
           ( psl-closing-brackets
             (+ (count ?} psl)
                (count ?\) psl)))
           ( psl-open-brackets (+ (count ?{ psl) (count ?\( psl)))
           ( psl-has-colon (plusp (count ?: psl)))
           (ppss (syntax-ppss))
           previous-comment-indent
           previous-line-was-comment
           this-line-comment-type
           pos)
      (cond ( ;; If "outside" always indent to 0
             (zerop (nth 0 ppss)) 0)
            ( ;; If is inside a comment
             ( eq this-line-comment-type t)
             (save-excursion
               (nth 4 ppss)
              (setq pos (point))
              (forward-line -1)
              (skip-chars-forward " \t")
              (if (>= (nth 8 ppss) (point))
                  (progn
                    (goto-char (nth 8 ppss))
                    (if (eq (char-after pos) ?*)
                        (forward-char 1)
                        (if (not (looking-at comment-start-skip))
                            (error "Internal css-mode error")
                            (goto-char (match-end 0))))
                    (current-column))
                  (current-column))))
           ( ;; End of colon block
            (and (not psl-has-colon)
                 ;; After sass @include
                 (not (equal psl-first-char ?@ ))
                 (equal psl-last-char ?\;)
                 (zerop psl-open-brackets)
                 (zerop psl-closing-brackets)
                 (not (member (char-after (es-indentation-end-pos))
                              '( ?\} ?\) ) )))
            (when es-css-debug (message "EOC o:%s c:%s" psl-open-brackets psl-closing-brackets))
            (max 0 (- psl-indent css-indent-offset)))
           ( ;; Not-first member of comma ending lines
            (and (not (search ": " psl))
                 (equal psl-last-char ?\, )
                 (zerop psl-open-brackets)
                 (zerop psl-closing-brackets))
            (when es-css-debug (message "MCB o:%s c:%s" psl-open-brackets psl-closing-brackets))
            psl-indent)
           ( ;; Line after beginning of comma block
            (and (member psl-last-char '( ?: ?\, ) )
                 ;; Hack
                 ;; (search ": " psl)
                 ;; (not (equal psl-last-char ?\;))
                 (every 'zerop
                        (list psl-open-brackets
                              psl-closing-brackets)))
            (when es-css-debug (message "LABOC <%s>" (char-to-string psl-last-char)))
            (+ psl-indent css-indent-offset))
           ( t (* (- (nth 0 ppss)
                     (if (member (char-after (es-indentation-end-pos))
                                 '( ?\} ?\) ) )
                         1 0))
                  css-indent-offset))))))

(defun* es-css-indent-line ()
  (interactive)
  (save-excursion
    (indent-line-to (es-css-calculate-indent)))
  (when (< (current-column) (current-indentation))
    (back-to-indentation))
  (return-from es-css-indent-line)
  (save-excursion
    (es-css-calculate-indent)

    (cond ( (= (line-number-at-pos) 1)
            (indent-line-to 0))
          ( (es-css-comment-line-p)
            (es-css-calculate-indent))
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