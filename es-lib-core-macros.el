(defmacro es-silence-messages (&rest body)
  `(flet ((message (&rest ignore)))
     ,@body))

(defmacro es-while-point-moving (&rest rest)
  (let ((old-point (gensym)))
    `(let (,old-point)
       (while (not (equal (point) ,old-point))
         (setq ,old-point (point))
         ,@rest))))

(defmacro es-neither (&rest args)
  `(not (or ,@args)))

(defmacro es-define-buffer-local-vars (&rest list)
  "Syntax example:
\(es-define-buffer-local-vars
 mvi-current-image-file nil\)"
  (let (result)
    (while list
      (let ((name (pop list))
            (value (pop list)))
        (push `(defvar ,name ,value) result)
        (push `(make-variable-buffer-local (quote ,name)) result)))
    (cons 'progn (nreverse result))))

(defmacro es-back-pop (symbol)
  (let ( (result (gensym)))
    `(let ( (,result (first (last ,symbol))))
       (setq ,symbol (butlast ,symbol))
       ,result)))

(defmacro es-back-push (what where)
  `(setq ,where (append ,where (list ,what))))

(provide 'es-lib-core-macros)