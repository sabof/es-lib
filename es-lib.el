(defun es-buffer-mode (buffer-or-name)
  (with-current-buffer (get-buffer buffer-or-name)
    major-mode))

(defun es-mapbuffer (function list)
  (mapcar (lambda (buf)
            (with-current-buffer buf
              (funcall function buf)))
          list))

(defun es-buffers-where-local-variable-is (var-sym value)
  (remove-if-not (lambda (buf)
                   (with-current-buffer buf
                     (equal (symbol-value var-sym)
                            value)))
                 (buffer-list)))

(defmacro es-silence-messages (&rest body)
  `(flet ((message (&rest ignore)))
     ,@body))

(defun es-string-begins-with-p (string beginning)
  "Return t if and only if string begins with beginning"
  (string-match-p (concat "^" (regexp-quote beginning)) string))

(defun es-string-remove-properties (string)
  (with-temp-buffer
    (insert string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun* es-set-region (point mark)
  (interactive)
  (push-mark mark)
  (goto-char point)
  (activate-mark)
  (setq deactivate-mark nil))

(defun es-line-matches-p (regexp)
  "*****"
  (string-match-p
   regexp
   (buffer-substring
    (line-beginning-position)
    (line-end-position))))

(defun es-line-visible-p ()
  (not (es-line-matches-p "^[\t ]*$")))

(defun es-line-empty-p ()
  (es-line-matches-p "^[ 	]*$"))

(defun es-replace-regexp-prog (regexp replacement &optional from to)
  "*****"
  (assert (or (neither from to) (and from to)))
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (and from to)
          (narrow-to-region from to))
        (while (re-search-forward regexp nil t)
          (replace-match replacement t nil))))))

(defun es-replace-prog (original replacement &optional from to)
  "*****"
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (and from to)
          (narrow-to-region from to))
        (while (search-forward original nil t)
          (replace-match replacement t nil))))))

(defun es-find-function-bound-to (key-sequence)
  "*****"
  (interactive "kFind function bound to: ")
  (let ((symbol (key-binding key-sequence)))
    (if (fboundp symbol)
        (find-function symbol)
        (message "Key sequence unbound"))))

(defun es-add-semicolon-at-eol ()
  (interactive)
  (add-at-eol ";")
  (fai-test-and-indent))

(defun es-add-comma-at-eol ()
  (interactive)
  (add-at-eol ","))

(defun es-buffers-with-mode (mode)
  (remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (eq major-mode mode)))
   (buffer-list)))

(defun es-push-line ()
  "beginning-of-line + open line."
  (interactive)
  (cond ( (> (line-number-at-pos) 1)
          (goto-char
           (min (save-excursion
                  (forward-line -1)
                  (point))
                (save-excursion
                  (let ((line-move-visual t))
                    (next-line -1))
                  (point))))
          (funcall (key-binding (kbd "C-e")))
          (end-of-line)
          (funcall (or (key-binding (kbd "<return>"))
                       (key-binding (kbd "\r"))
                       (key-binding (kbd "RET"))
                       'newline)))
        ( t
          (beginning-of-line)
          (newline)
          (backward-char)
          (when (bound-and-true-p fai-mode)
            (fai-test-and-indent)))))

(defun es-jump-line ()
  "end-of-line + newline."
  (interactive)
  ;; (end-of-line)
  (funcall (or (key-binding (kbd "C-e"))
               'end-of-line))
  (funcall (or (key-binding (kbd "<return>"))
               (key-binding (kbd "\r"))
               (key-binding (kbd "RET"))
               'newline)))

(defun es-new-empty-buffer ()
  "*****"
  (interactive)
  (if (or t (> (length (window-list)) 1))
      (switch-to-buffer (generate-new-buffer "untitled"))
      (pop-to-buffer (generate-new-buffer "untitled")))
  (lisp-interaction-mode))


(defun* es-define-keys (keymap &rest bindings)
  "Returns the keymap in the end.
*****"
  (while bindings
    (define-key keymap (pop bindings) (pop bindings)))
  keymap)
(put 'es-define-keys 'common-lisp-indent-function
     '(4 &body))

(provide 'es-lib)