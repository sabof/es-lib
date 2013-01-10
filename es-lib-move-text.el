(defun es--current-mode-indent-step ()
  (case major-mode
    (haskell-mode 1)
    (python-mode 4)
    (php-mode 4)
    (otherwise 2)))

(defun es--section-marking-end-of-line (&optional pos)
  (save-excursion
    (when pos
      (goto-char pos))
    (if (and (region-active-p) (equal (current-column) 0))
        (point)
        (min (point-max) (1+ (es-total-line-end-position))))))

(defun* es--move-text-internal (arg)
  (let* (( was-active (region-active-p))
         ( first-line-was-folded
           (save-excursion
             (when was-active
               (goto-char (region-beginning)))
             (es-line-folded-p)))
         ( initial-column (current-column))
         ( start (es-total-line-beginning-position
                  (if was-active
                      (region-beginning)
                      (point))))
         ( end (es--section-marking-end-of-line
                (if was-active
                    (region-end)
                    (point))))
         ( text (delete-and-extract-region start end))
         new-start)
    (es-total-forward-line arg)
    (setq new-start (point))
    (insert text)
    (unless (equal (aref text (1- (length text)))
                   (aref "\n" 0))
      (insert "\n"))
    (set-mark new-start)
    (exchange-point-and-mark)
    (if (or was-active first-line-was-folded)
        (setq deactivate-mark nil
              cua--explicit-region-start nil)
        (progn (move-to-column initial-column t)
               (deactivate-mark)))
    (and first-line-was-folded
         (fboundp 'fold-dwim-hide)
         (save-excursion
           (cond ( (memq major-mode
                         '(lisp-mode
                           emacs-lisp-mode
                           lisp-interaction-mode
                           common-lisp-mode))
                   (fold-dwim-hide))
                 ( (progn (goto-char (line-end-position))
                          (equal (char-before) ?\{))
                   (fold-dwim-hide)
                   ))))))

(defun* es--indent-rigidly-internal (arg)
  (cond ( (region-active-p)
          (let (( start
                  (es-total-line-beginning-position
                   (region-beginning)))
                ( end
                  (es--section-marking-end-of-line
                   (region-end))))
            (set-mark end)
            (goto-char start)
            (indent-rigidly start end arg)
            (setq deactivate-mark nil)))
        ( (es-line-empty-p)
          (let* (( cur-column (current-column))
                 ( step (abs arg))
                 ( rest (mod cur-column step))
                 ( new-indent
                   (max 0 (if (zerop rest)
                              (+ cur-column arg)
                              (if (plusp arg)
                                  (+ cur-column rest)
                                  (- cur-column (- step rest)))))))
            (if (> new-indent cur-column)
                (indent-to new-indent)
                (goto-char (+ new-indent (line-beginning-position)))
                (delete-region (point) (line-end-position))
                )))
        ( t (indent-rigidly
             (es-total-line-beginning-position (point))
             (es--section-marking-end-of-line (point))
             arg))))

(defun es-move-text-down ()
  "Move region or the current line down."
  (interactive)
  (es--move-text-internal 1))

(defun es-move-text-up ()
  "Move region or the current line up."
  (interactive)
  (es--move-text-internal -1))

(defun es-move-text-left ()
  "Move region or the current line left."
  (interactive)
  (es--indent-rigidly-internal
   (* -1 (es--current-mode-indent-step))))

(defun es-move-text-right ()
  "Move region or the current line right."
  (interactive)
  (es--indent-rigidly-internal
   (es--current-mode-indent-step)))

(provide 'es-lib-move-text)