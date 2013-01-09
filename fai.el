(defvar fai-indent-function 'fai-test-and-indent)
(defvar fai-indentable-line-p-function (lambda () t))
(defvar fai-after-change-indentation t)
(defvar fai-indent-limit 20)
(defvar fai-timer nil)

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
      (unless (or dont-indent
                  (> (- (point) starting-point)
                     4000))
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
               (es-point-between-pairs-p))
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
    (dotimes (ignore fai-indent-limit)
      (forward-line)
      (fai-test-and-indent))))

(defun fai-reindent-defun ()
  "Currently only works for lisp"
  (let (init-pos
        line-end-distance)
    (condition-case nil
        (save-excursion
          (setq line-end-distance)
          (beginning-of-defun)
          (setq init-pos (point))
          (forward-sexp)
          (when (> (1+ (- (line-number-at-pos)
                          (line-number-at-pos init-pos)))
                   fai-indent-limit)
            (error "defun too long"))
          (goto-char init-pos)
          (indent-pp-sexp))
      (error (fai-indent-forward)))
    (when (< (point) indentation-beginning)
      (goto-char indentation-beginning))
    ;; (fai-correct-position-this)
    ))

(defun fai-delete-indentation ()
  (interactive)
  (delete-indentation)
  (fai-test-and-indent))

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

(defun fai-correct-position-this (&optional maybe-change-line)
  ;; Actually useless without maybe-change-line
  (let (( init-pos (point))
        ( indentation-beginning
          (es-indentation-end-pos)))
    (if (and maybe-change-line
             (> indentation-beginning init-pos))
        (cond ( (memq this-command '(backward-char left-char))
                (end-of-line 0))
              ( (memq this-command '(forward-char right-char))
                (back-to-indentation))
              ))))

(defun fai-before-change-hook (&rest ignore)
  "Change tracking."
  (when fai-after-change-indentation
    (setq fai-change-flag t)))

(defun* fai-post-command-hook ()
  "First key stroke tracking, cursor correction"
  (when (or (not fai-mode))
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
               (memq this-command
                     '(backward-char forward-char
                       left-char right-char
                       previous-line next-line)))
      (fai-correct-position-this t))
    (fai-indent-ontimer-embedded)
    ))

(defun fai-indent-ontimer ()
  "Tests and indentation"
  (when (and fai-change-flag
             (buffer-modified-p)
             fai-mode
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
    (funcall fai-indent-function)
    ;; (message "indent")
    )
  (setq fai-change-flag nil)
  )

(defun fai-indent-ontimer-embedded ()
  "Tests and indentation"
  (when (and fai-change-flag
             (buffer-modified-p)
             fai-mode
             (or fai-first-keystroke
                 (not (memq
                       this-command
                       '(save-buffer
                         delete-horizontal-space
                         undo
                         undo-tree-undo
                         undo-tree-redo
                         quoted-insert
                         backward-paragraph
                         self-insert-command))))
             (not (region-active-p)))
    (funcall fai-indent-function)
    ;; (message "indent")
    )
  (setq fai-change-flag nil)
  )

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
  (pushnew 'fai-before-change-hook before-change-functions)
  (add-hook 'post-command-hook 'fai-post-command-hook t t)
  (setq fai-timer
        (run-with-idle-timer 0 t 'fai-indent-ontimer))
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
      (fai--init)))

(provide 'fai)