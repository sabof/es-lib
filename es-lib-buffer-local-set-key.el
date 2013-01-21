(defvar es-buffer-local-mode nil)
(make-variable-buffer-local 'es-buffer-local-mode)

(defun* es-buffer-local-set-key (key action)
  (when es-buffer-local-mode
    (define-key (es-mode-keymap es-buffer-local-mode)
        key action)
    (return-from es-buffer-local-set-key))
  (let* ((mode-name-loc (gensym "-blm")))
    (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
    (setq es-buffer-local-mode mode-name-loc)
    (funcall mode-name-loc 1)
    (define-key (es-mode-keymap mode-name-loc) key action)))

(defun* es-buffer-local-set-keys (&rest bindings)
  (while bindings
    (es-buffer-local-set-key (pop bindings) (pop bindings))))
(put 'es-buffer-local-set-keys
     'common-lisp-indent-function
     '(&body))

(provide 'es-lib-buffer-local-set-key)
;; es-lib-buffer-local-set-key.el ends here