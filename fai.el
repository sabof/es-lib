(defvar fai-indent-function 'fai-test-and-indent)
(make-variable-buffer-local 'fai-indent-function)

(defvar fai-indentable-line-p-function (lambda () t))
(make-variable-buffer-local 'fai-indentable-line-p-function)

(defvar fai-after-change-indentation t)
(make-variable-buffer-local 'fai-after-change-indentation)

(defvar fai-change-flag nil)
(make-variable-buffer-local 'fai-change-flag)

(defvar fai-first-keystroke nil)
(make-variable-buffer-local 'fai-first-keystroke)

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
          (end-point (- (line-end-position) (point))))
      (yank)
      (unless dont-indent
        (indent-region starting-point (point)))
      (goto-char (- (line-end-position) end-point)))
    (fai-test-and-indent)))

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

(defun fai-last-character-pos ()
  (save-excursion
    (end-of-line)
    (let (current-pos)
      (while (and (not (equal (point) current-pos))
                  (> (current-column) 0)
                  (equal (char-before) (aref " " 0)))
        (setq current-pos (point))
        (backward-char)))
    (point)))

(defun fai-end ()
  (interactive)
  (if truncate-lines
      (end-of-visual-line)
      (end-of-line)))

(defun fai-end-with-region ()
  "Ex. Can be bound to <S-end>"
  (interactive)
  (unless (region-active-p)
    (set-mark (point)))
  (fai-end))

(defun fai-home ()
  (interactive)
  (let* (old-point
         same-line
         ( beginning
           (save-excursion
             (beginning-of-line)
             (setq old-point (point))
             (beginning-of-visual-line)
             (setq same-line (eq old-point (point)))
             (indentation-end-at-pos))))
    (if same-line
        (cond ((eq (point) (indentation-end-at-pos))
               (beginning-of-line))
              (t (back-to-indentation)))
        (goto-char beginning))))

(defun fai-after-last-char-p ()
  (>= (point) (fai-last-character-pos)))

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
  (let ((was-at-eol (>= (point) (fai-last-character-pos))))
    (save-excursion
      (newline))
    (fai-test-and-indent)
    (save-excursion
      (forward-char)
      (indent-according-to-mode))))

(defun fai-indent-forward ()
  (let (( lines-to-indent 16)
        ( current-line (line-number-at-pos))
        ( buffer-end-line
          (line-number-at-pos
           (buffer-end 1))))
    (msave-excursion
     (fai-test-and-indent)
     (loop repeat (min lines-to-indent
                       (- buffer-end-line current-line))
           do (next-logical-line)
           (fai-test-and-indent)))))

(defun fai-back-to-indentation-with-region ()
  (interactive)
  (unless (region-active-p)
    (set-mark (point)))
  (fai-home))

(defun* fai-newline-and-indent ()
  (interactive)
  (cond ( (and (equal (char-before) ?{ )
               (equal (char-after) ?} ))
          (progn (newline)
                 (save-excursion
                   (newline))
                 (indent-according-to-mode)
                 (save-excursion
                   (forward-char)
                   (indent-according-to-mode))
                 (return-from fai-newline-and-indent))))
  (when (region-active-p)
    (delete-region (point) (mark)))
  (newline)
  (fai-test-and-indent)
  (when (memq major-mode '(nxml-mode web-mode))
    (msave-excursion
     (forward-line -1)
     (indent-according-to-mode))))

(defun fai-make-keymap ()
  (define-keys (make-sparse-keymap)
    [mouse-2] 'fai-mouse-yank
    "\C-v" 'fai-indented-yank
    "\r" 'fai-newline-and-indent
    [remap open-line] 'fai-open-line
    [remap delete-char] 'fai-delete
    [remap paredit-forward-delete] 'fai-delete
    (kbd "<backspace>") 'fai-backspace
    ;; META COMMENT:
    ;; I have no idea what I was talking about
    ;;
    ;; Doesn't work because superseded my visual-line-mode
    ;; Solutions:
    ;; * Go back to soft-wrap line
    ;; * Find a way to disable remapped keybindings
    [remap beginning-of-line] 'fai-home  ; program-only func?
    [remap beginning-of-visual-line] 'fai-home
    [remap move-beginning-of-line] 'fai-home
    (kbd "S-<home>") 'fai-back-to-indentation-with-region))

(defun fai-correct-position-this (&optional dont-change-line)
  (flet ( (exec-in-dynamic-enviroment (thunk)
            (let* (( init-pos (point))
                   ( indentation-beginning
                     (indentation-end-at-pos))
                   ( last-character
                     (fai-last-character-pos)))
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

(defun* fai-post-command-hook ()
  "First key stroke tracking, cursor correction"
  (let ( (last-input-structural
          (member last-input-event
                  (mapcar (lambda (string) (aref string 0))
                          (list "(" ")" "[" "]" "{" "}" "," ";" " ")))
          ))
    (unless fai-mode
      (return-from fai-post-command-hook))
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

(define-minor-mode fai-mode
    "Fuchikoma Automatic Indentation"
  nil " fai" (fai-make-keymap)
  (eval-after-load "multiple-cursors-core"
    '(pushnew 'fai-mode mc/unsupported-minor-modes))
  (when (boundp 'visual-line-mode-map)
    (define-key visual-line-mode-map [remap move-beginning-of-line] nil))
  (if fai-mode
      (progn
        (setq inhibit-modification-hooks nil)
        (define-key cua--region-keymap [remap delete-char]
          (lambda ()
            (interactive)
            (if fai-mode
                (fai-delete)
                (cua-delete-region))))
        (pushnew 'fai-indent-changehook before-change-functions)
        (add-hook 'post-command-hook 'fai-post-command-hook t t)
        (run-with-idle-timer 0 t 'fai-indent-ontimer)
        )))

(provide 'fai)