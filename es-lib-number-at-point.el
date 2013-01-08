(defun es-number-at-point ()
  (when (looking-at "[[:digit:]-]+")
    (save-excursion
      (while (looking-at "[[:digit:]-]+")
        (backward-char))
      (list (match-string-no-properties 0)
            (match-beginning 0)
            (match-end 0)))))

(defun* es--change-number-at-point (&optional decrease)
  (let ((number (es-number-at-point)))
    (if (not number)
        (progn
          (save-excursion
            (when (re-search-backward "[0-9]" (line-beginning-position) t)
              (es--change-number-at-point decrease)))
          (multiple-value-bind (num-string beg end) (es-number-at-point)
            (when (and (numberp beg)
                       (equal (- end beg) 1))
              (forward-char))))
        (multiple-value-bind (num-string beg end) number
          ;; number
          (let* ((start-pos (point))
                 (distance-from-end (- end start-pos))
                 (increment (* (expt 10 (1- distance-from-end))
                               (if decrease -1 1)))
                 (result (+ (string-to-number num-string) increment))
                 (result-string (number-to-string
                                 result)))
            (delete-region beg end)
            (insert-string result-string)
            (goto-char (max beg
                            (+ start-pos
                               (- (length result-string)
                                  (length num-string))))))))))

(defun es-increase-number-at-point ()
  "Increases the digit at point.
The increment some power of 10, depending on the positon of the cursor. If there
is no number at point, will try to increment the previous number on the same
line."
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es--change-number-at-point)))

(defun es-decrease-number-at-point ()
  "See documentation for `es-increase-number-at-point'."
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es--change-number-at-point t)))

(provide 'es-lib-number-at-point)