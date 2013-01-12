(require 'css-mode)
(require 'flymake)

;; For es-sass-mode
(defvar es-css-comment-line-p-function 'es-css-comment-line-p
  "Should return 1 if at the beginning of a comment, t if inside")

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
              (funcall es-css-comment-line-p-function))))

(defvar es-css-debug nil)

(defun es-css-indent-debug (name)
  (when es-css-debug
    (message "%s" name)))

(defun es-css-calculate-indent ()
  ;; If I go to the beginning of line, MC stops working
  (back-to-indentation)
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
           pos)
      (cond ( ;; Inside a multiline comment
             ( eq (funcall es-css-comment-line-p-function) t)
             (es-css-indent-debug "MC")
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
            ( ;; If "outside" indent to 0
             (zerop (nth 0 ppss))
             (es-css-indent-debug "ZERO")
             0)
            ( ;; End of colon block
             (and (not psl-has-colon)
                  ;; Not after sass @include
                  (not (equal psl-first-char ?@ ))
                  (equal psl-last-char ?\;)
                  (zerop psl-open-brackets)
                  (zerop psl-closing-brackets)
                  (not (member (char-after (es-indentation-end-pos))
                               '( ?\} ?\) ) )))
             (es-css-indent-debug "EOC")
             (max 0 (- psl-indent css-indent-offset)))
            ( ;; Not-first member of comma ending lines
             (and (not (search ": " psl))
                  (equal psl-last-char ?\, )
                  (zerop psl-open-brackets)
                  (zerop psl-closing-brackets))
             (es-css-indent-debug "MCB")
             psl-indent)
            ( ;; Line after beginning of comma block
             (and (member psl-last-char '( ?: ?\, ) )
                  ;; Hack
                  ;; (search ": " psl)
                  ;; (not (equal psl-last-char ?\;))
                  (every 'zerop
                         (list psl-open-brackets
                               psl-closing-brackets)))
             (es-css-indent-debug "LABOC")
             (+ psl-indent css-indent-offset))
            ( ;; Default, based on nesting level
             t
             (es-css-indent-debug "LAST")
             (* (- (nth 0 ppss)
                   (if (member (char-after (es-indentation-end-pos))
                               '( ?\} ?\) ) )
                       1 0))
                css-indent-offset))))))

(defun es-css-set-fl-keywords ()
  (setq css-font-lock-keywords
        `(("!\\s-*important" . font-lock-builtin-face)
          ;; Atrules keywords.  IDs not in css-at-ids are valid (ignored).
          ;; In fact the regexp should probably be
          ;; (,(concat "\\(@" css-ident-re "\\)\\([ \t\n][^;{]*\\)[;{]")
          ;;  (1 font-lock-builtin-face))
          ;; Since "An at-rule consists of everything up to and including the next
          ;; semicolon (;) or the next block, whichever comes first."
          (,(concat "@" css-ident-re) . font-lock-builtin-face)
          ;; Selectors.
          ;; FIXME: attribute selectors don't work well because they may contain
          ;; strings which have already been highlighted as f-l-string-face and
          ;; thus prevent this highlighting from being applied (actually now that
          ;; I use `append' this should work better).  But really the part of hte
          ;; selector between [...] should simply not be highlighted.
          (,(concat "^\\([ \t]*[^@:{}\n][^:{}]+\\(?::" (regexp-opt css-pseudo-ids t)
                    "\\(?:([^)]+)\\)?[^:{\n]*\\)*\\)\\(?:\n[ \t]*\\)*{")
            (1 'css-selector append))
          ;; In the above rule, we allow the open-brace to be on some subsequent
          ;; line.  This will only work if we properly mark the intervening text
          ;; as being part of a multiline element (and even then, this only
          ;; ensures proper refontification, but not proper discovery).
          ("^[ \t]*{" (0 (save-excursion
                           (goto-char (match-beginning 0))
                           (skip-chars-backward " \n\t")
                           (put-text-property (point) (match-end 0)
                                              'font-lock-multiline t)
                           ;; No face.
                           nil)))
          ;; Properties.  Again, we don't limit ourselves to css-property-ids.
          (,(concat "\\(?:[{;]\\|^\\)[ \t]*\\("
                    "\\(?:\\(" css-proprietary-nmstart-re "\\)\\|"
                    css-nmstart-re "\\)" css-nmchar-re "*"
                    "\\)\\s-*:")
            (1 (if (match-end 2) 'css-proprietary-property 'css-property))))))

(defun* es-css-indent-line ()
  (interactive)
  (save-excursion
    (indent-line-to (es-css-calculate-indent)))
  (when (< (current-column) (current-indentation))
    (back-to-indentation)))

(defun* es-css-beginning-of-defun (&optional (arg 1))
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

(defun* es-css-end-of-defun (&optional (arg 1))
  (interactive)
  (ignore-errors
    (if (es-css-inside-block)
        (es-css-beginning-of-defun))
    (progn
      (re-search-forward "{" nil t arg)
      (backward-char)
      (forward-sexp)
      (ignore-errors
        (forward-char)))
    t))

;;;###autoload
(define-derived-mode es-css-mode css-mode
  "ES CSS Mode " "Mode for modern CSS"
  (set (make-local-variable 'indent-line-function)
       'es-css-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'es-css-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'es-css-end-of-defun)
  (set (make-local-variable 'es-aai-indent-function)
       'es-aai-indent-defun) )

(provide 'es-css-mode)