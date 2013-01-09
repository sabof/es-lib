(defvar fai-indent-function 'fai-test-and-indent)
(defvar fai-indentable-line-p-function (lambda () t))
(defvar fai-after-change-indentation t)
(defvar fai-indent-forward-lines 16)

(es-define-buffer-local-vars
 fai-change-flag nil
 fai-first-keystroke nil)

(defun fai-test-and-indent ()
  "Run some basic checks (including whether fai-mode is on),
and (indent-according-to-mode)."
  (when (and fai-mode
             (not (eq indent-line-function 'insert-tab))
             (funcall fai-indentable-line-p-function))
    (indent-according-to-mode)))

(defun fai-indented-yank (&optional dont-indent)
  (interactive)
  (flet ((message (&rest ignore)))
    (when (region-active-p)
      (delete-region (point) (mark)))
    (let ((starting-point (point))
          end-distance
          line)
      (yank)
      (setq end-distance (- (line-end-position) (point))
            line (line-number-at-pos))
      (unless dont-indent
        (indent-region starting-point (point)))
      (when (bound-and-true-p font-lock-mode)
        (font-lock-fontify-region starting-point (point)))
      (goto-line line)
      (goto-char (- (line-end-position) end-distance))
      ;; (fai-test-and-indent)
      )))

(defun fai-mouse-yank (event &optional dont-indent)
  (interactive "e")
  (if (region-active-p)
      (let ((reg-beg (region-beginning))
            (reg-end (region-end)))
        (mouse-set-point event)
        (when (and (<= reg-beg (point))
                   (<= (point) reg-end))
          (delete-region reg-beg reg-end)
          (goto-char reg-beg)))
      (progn
        (mouse-set-point event)
        (deactivate-mark)))
  (fai-indented-yank dont-indent))

(defun fai-mouse-yank-dont-indent (event)
  (interactive "e")
  (fai-mouse-yank event t))

(defun fai-delete (&optional from-backspace)
  (interactive)
  (if (region-active-p)
      (delete-region (point) (mark))
      ;; The following functionality might overlap;
      (if (fai-after-last-char-p)
          (progn
            (fai-correct-position-this)
            (delete-region (point) (1+ (line-end-position)))
            (when (and (fixup-whitespace)
                       (not from-backspace))
              (backward-char))
            (fai-test-and-indent))
          (delete-char 1)))
  (when fai-after-change-indentation
    (fai-test-and-indent)))

(defun fai-after-last-char-p ()
  (>= (point) (es-visible-end-of-line)))

(defun fai-before-or-at-indentation-p ()
  (<= (current-column) (current-indentation)))

(defun fai-backspace ()
  (interactive)
  (cond ( (and (not (bound-and-true-p autopair-mode))
               (point-between-pairs-p))
          (delete-char 1)
          (delete-char -1))
        ( (region-active-p)
          (delete-region (point) (mark)))
        ( (fai-before-or-at-indentation-p)
          (previous-logical-line)
          (end-of-line)
          (fai-delete t)
          (fai-test-and-indent))
        ( (bound-and-true-p paredit-mode)
          (paredit-backward-delete))
        ( t (backward-delete-char 1)))
  (when fai-after-change-indentation
    (fai-test-and-indent)))

(defun fai-open-line ()
  (interactive)
  (let ((was-at-eol (>= (point) (es-visible-end-of-line))))
    (save-excursion
      (newline))
    (fai-test-and-indent)
    (save-excursion
      (forward-char)
      (indent-according-to-mode))))

(defun fai-indent-forward ()
  (save-excursion
    (fai-test-and-indent)
    (dotimes (ignore fai-indent-forward-lines)
      (forward-line)
      (fai-test-and-indent))))

(defun fai-reindent-defun ()
  (save-excursion
    (beginning-of-defun)
    (indent-pp-sexp)))

(defun* fai-newline-and-indent ()
  (interactive)
  (when (and (not (region-active-p))
             (equal (char-before) ?{ )
             (equal (char-after) ?} ))
    (newline)
    (save-excursion
      (newline))
    (indent-according-to-mode)
    (save-excursion
      (forward-char)
      (indent-according-to-mode))
    (return-from fai-newline-and-indent))
  (when (region-active-p)
    (delete-region (point) (mark)))
  (newline)
  (fai-test-and-indent)
  (when (memq major-mode '(nxml-mode web-mode))
    (save-excursion
      (forward-line -1)
      (indent-according-to-mode))))

(defun fai-correct-position-this (&optional dont-change-line)
  ;; Should be a macrolet
  (flet ( (exec-in-dynamic-enviroment (thunk)
            (let* (( init-pos (point))
                   ( indentation-beginning
                     (es-indentation-end-pos))
                   ( last-character
                     (es-visible-end-of-line)))
              (funcall thunk))))
    ;; next/prev line
    (exec-in-dynamic-enviroment
     (lambda ()
       (when (and (memq this-command '(backward-char left-char))
                  (> indentation-beginning init-pos))
         (end-of-line 0))))
    (exec-in-dynamic-enviroment
     (lambda ()
       (when (< (point) indentation-beginning)
         (goto-char indentation-beginning))))))

(defun fai-correct-position-this (&optional dont-change-line)
  ;; Should be a macrolet
  (macrolet ( (exec-with-vars (&rest body)
                (let* (( init-pos (point))
                       ( indentation-beginning
                         (es-indentation-end-pos))
                       ( last-character
                         (es-visible-end-of-line)))
                  ,@body)))
    ;; next/prev line
    (exec-with-vars
     (when (and (memq this-command '(backward-char left-char))
                (> indentation-beginning init-pos))
       (end-of-line 0)))
    (exec-with-vars
     (when (< (point) indentation-beginning)
       (goto-char indentation-beginning)))))

(defun fai-indent-ontimer ()
  "Tests and indentation"
  (when (and fai-change-flag
             (buffer-modified-p)
             (bound-and-true-p fai-mode)
             (or fai-first-keystroke
                 (not (memq
                       last-command
                       '(save-buffer
                         delete-horizontal-space
                         undo
                         undo-tree-undo
                         undo-tree-redo
                         quoted-insert
                         backward-paragraph
                         self-insert-command))))
             (not (region-active-p)))
    (funcall fai-indent-function))
  (setq fai-change-flag nil))

(defun fai-indent-changehook (&optional a b c)
  "Change tracking."
  (when fai-after-change-indentation
    (setq fai-change-flag t)))

(defun fai-delete-indentation ()
  (interactive)
  (delete-indentation)
  (fai-test-and-indent))

(defun* fai-post-command-hook ()
  "First key stroke tracking, cursor correction"
  (when (or (not fai-mode)
            )
    (return-from fai-post-command-hook))
  (let ( (last-input-structural
          (member last-input-event
                  (mapcar 'string-to-char
                          (list "(" ")" "[" "]" "{" "}" "," ";" " ")))))
    (setq fai-first-keystroke
          (and (eq this-command 'self-insert-command)
               (or last-input-structural
                   (not (eq last-command 'self-insert-command)))))
    (when (and (not (region-active-p))
               (not cua--rectangle)
               (member this-command
                       '(backward-char forward-char
                         left-char right-char
                         previous-line next-line)))
      (fai-correct-position-this))))

(defun fai--init ()
  (eval-after-load "multiple-cursors-core"
    '(pushnew 'fai-mode mc/unsupported-minor-modes))
  (eval-after-load "paredit"
    '(es-define-keys fai-mode-map
      [remap paredit-forward-delete] 'fai-delete
      [remap paredit-backward-delete] 'fai-backspace))
  (eval-after-load "cua-base"
    (define-key cua--region-keymap [remap delete-char]
      (lambda ()
        (interactive)
        (if fai-mode
            (fai-delete)
            (cua-delete-region)))))
  (setq inhibit-modification-hooks nil)
  (add-hook 'fai-indent-changehook 'before-change-functions t t)
  (add-hook 'post-command-hook 'fai-post-command-hook t t)
  (run-with-idle-timer 0 t 'fai-indent-ontimer)
  (es-define-keys fai-mode-map
    [mouse-2] 'fai-mouse-yank
    "\C-v" 'fai-indented-yank
    [remap newline] 'fai-newline-and-indent
    [remap delete-indentation] 'fai-delete-indentation
    [remap open-line] 'fai-open-line
    [remap delete-char] 'fai-delete
    [remap forward-delete] 'fai-delete
    [remap backward-delete-char-untabify] 'fai-backspace
    [remap backward-delete-char] 'fai-backspace
    ))

(define-minor-mode fai-mode
    "Fuchikoma Automatic Indentation"
  nil " fai" (make-sparse-keymap)
  (if fai-mode
      (progn
        )))

(provide 'fai)