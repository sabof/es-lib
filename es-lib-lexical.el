;;; -*- lexical-binding: t -*-

(defun es-back-curry (func &rest more-args)
  "Like (apply-partially), but adds arguments to the end."
  (lambda (&rest args)
    (apply func (append args more-args))))

(defun es-comp (&rest funcs)
  "Same as clojure's (comp)"
  (lambda (arg)
    (let ((arg arg)
          (funcs (reverse funcs)))
      (mapc
       (lambda (func)
         (setq arg (funcall func arg)))
       funcs)
      arg)))

(defun es-complement (func)
  "Same as clojure's (complement)"
  (lambda (&rest args)
    (not (apply func args))))

(defun es-constantly (arg)
  "Same as clojure's (constantly)"
  (lambda (&rest args)
    arg))

(defun es-flip (func)
  "Creates a function with FUNC's arguments reversed."
  (lambda (&rest args)
    (apply func (reverse args))))

(defun* es-make-timer-buffer (time-limit)
  "Accepts a time-limit in minutes."
  (interactive (list (read-number "Time limit: ")))
  (let (( start-time (current-time))
        ( buf (generate-new-buffer "*timer*"))
        time-difference
        the-timer)
    (setq the-timer
          (run-with-timer
           0 1 (lambda ()
                 (block ablock
                   (unless (buffer-live-p buf)
                     (cancel-timer the-timer)
                     (return-from ablock))
                   (setq time-difference
                         (time-subtract (current-time) start-time))
                   (with-current-buffer buf
                     (erase-buffer)
                     (insert
                      (if (> (time-to-seconds time-difference)
                             (* 60 time-limit))
                          (prog1 (format "%s minutes passed at: %s"
                                         time-limit
                                         (format-time-string
                                          "%H:%M"))
                            (cancel-timer the-timer))
                          (format "%s / %s:00"
                                  (format-time-string
                                   "%M:%S"
                                   time-difference)
                                  time-limit))))))))
    ;; Shouln't defvars be dynamically bound?
    (es-pop-to-buffer-vertically buf)
    (setq cursor-type nil)
    (window-resize nil (- (window-min-delta)))
    (set-window-dedicated-p nil t)
    (setq window-size-fixed t)))

(provide 'es-lib-lexical)