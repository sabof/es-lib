(defun es-duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let* (( pnt (point))
         ( start (es-total-line-beginning-position))
         ( end (es-total-line-end-position))
         ( copy-store (buffer-substring start end)))
    (goto-char end)
    (newline)
    (insert copy-store)
    (goto-char (+ 1 end (- pnt start)))))

(defun es-duplicate-region ()
  "Duplicate the active region."
  (interactive)
  (let (( copy-store
          (or (es-active-region-string)
              (error "No active region")))
        b-pos er-pos)
    (goto-char (region-end))
    ;; For things like (mark-paragraph)
    (unless (zerop (current-column))
      (newline))
    (set-mark (setq b-pos (point)))
    (insert copy-store)
    (setq er-pos (- (line-end-position) (point)))
    (unless (or (eq major-mode 'haskell-mode)
                (eq indent-line-function 'insert-tab))
      (indent-region b-pos (point)))
    (goto-char (- (line-end-position) er-pos))
    (activate-mark)
    (setq deactivate-mark nil
          cua--explicit-region-start nil)))

;;;###autoload
(defun es-duplicate-line-or-region ()
  (interactive)
  (if (region-active-p)
      (es-duplicate-region)
      (es-duplicate-line)))

(provide 'es-lib-duplicate)