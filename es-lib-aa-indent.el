(defvar es-aai-indent-function 'es-aai-indent-line-maybe
  "Function to call after ever change, when")
(defvar es-aai-indentable-line-p-function (es-constantly t)
  "For mode-specifc cusomizations.")
(defvar es-aai-after-change-indentation t
  "Whether to reindent after every change.
Useful when you want to keep the keymap and cursor repositioning.")
(defvar es-aai-indent-limit 20
  "Maximum number of lines for after-change indentation.")

(es-define-buffer-local-vars
 es-aai--change-flag nil)

(defun es-aai-indent-line-maybe ()
  "\(indent-according-to-mode\) when `es-aai-indentable-line-p-function' returns non-nil.
All indentation happends through this function."
  (when (and es-aai-mode
             (not (eq indent-line-function 'insert-tab))
             (funcall es-aai-indentable-line-p-function))
    (indent-according-to-mode)))

(defun es-aai-indent-forward ()
  "Indent current line, and \(1- `es-aai-indent-limit'\) lines afterwards."
  (save-excursion
    (loop repeat es-aai-indent-limit do
          (es-aai-indent-line-maybe)
          (forward-line))))

(defun* es-aai--indent-region (start end)
  "Indent region lines where `es-aai-indentable-line-p-function' returns non-nil."
  (save-excursion
    (let ((end-line (line-number-at-pos end)))
      (goto-char start)
      (while (<= (line-number-at-pos) end-line)
        (es-aai-indent-line-maybe)
        (when (plusp (forward-line))
          (return-from es-aai--indent-region))))))

(defun es-aai-indent-defun ()
  "Indents current defun, if it is smaller than `es-aai-indent-limit'.
Otherwise call `es-aai-indent-forward'."
  (let (init-pos
        end-pos
        line-end-distance)
    (condition-case nil
        (save-excursion
          (setq line-end-distance)
          ;; So you don't go to the previous defun, when already at the
          ;; beginnning. Works, given this mode cursor-correcting behaviour, and
          ;; assuming that defuns always start at first collumn.
          (unless (zerop (current-column))
            (beginning-of-defun))
          (setq init-pos (point))
          (end-of-defun)
          (when (> (1+ (- (line-number-at-pos)
                          (line-number-at-pos init-pos)))
                   es-aai-indent-limit)
            (error "defun too long"))
          (setq end-pos (point))
          (goto-char init-pos)
          (es-aai--indent-region init-pos end-pos))
      (error (es-aai-indent-forward)))
    (es-aai-correct-position-this)))

(defun es-aai-indented-yank (&optional dont-indent)
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
        (es-aai--indent-region starting-point (point)))

      ;; (when (bound-and-true-p font-lock-mode)
      ;;   (font-lock-fontify-region starting-point (point)))
      (goto-line line)
      (goto-char (- (line-end-position) end-distance)))))

(defun es-aai-mouse-yank (event &optional dont-indent)
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
  (es-aai-indented-yank dont-indent))

(defun es-aai-mouse-yank-dont-indent (event)
  (interactive "e")
  (es-aai-mouse-yank event t))

(defun es-aai-delete-char (&optional from-backspace)
  "Like `delete-char', but deletes indentation, if point is at it, or before it."
  (interactive)
  (if (region-active-p)
      (delete-region (point) (mark))
      (if (>= (point) (es-visible-end-of-line))
          (progn
            (delete-region (point) (1+ (line-end-position)))
            (when (and (es-fixup-whitespace)
                       (not from-backspace))
              (backward-char)))
          (delete-char 1))))

(defun es-aai-backspace ()
  "Like `backward-delete-char', but removes the resulting gap when point is at EOL."
  (interactive)
  (cond ( (region-active-p)
          (delete-region (point) (mark)))
        ( (and (not (bound-and-true-p autopair-mode))
               (es-point-between-pairs-p))
          (delete-char 1)
          (delete-char -1))
        ( (<= (current-column)
              (current-indentation))
          (forward-line -1)
          (goto-char (line-end-position))
          (es-aai-delete-char t))
        ( (bound-and-true-p paredit-mode)
          (paredit-backward-delete))
        ( t (backward-delete-char 1))))

(defun es-aai-open-line ()
  "Open line, and indent the following."
  (interactive)
  (let ((was-at-eol (>= (point) (es-visible-end-of-line))))
    (save-excursion
      (newline))
    (save-excursion
      (forward-char)
      (es-aai-indent-line-maybe))))

(defun* es-aai-newline-and-indent ()
  (interactive)
  ;; For c-like languages
  (when (and (not (region-active-p))
             (equal (char-before) ?{ )
             (equal (char-after) ?} ))
    (newline)
    (save-excursion
      (newline))
    (es-aai-indent-line-maybe)
    (save-excursion
      (forward-char)
      (es-aai-indent-line-maybe))
    (return-from es-aai-newline-and-indent))
  (when (region-active-p)
    (delete-region (point) (mark)))
  (newline)
  (es-aai-indent-line-maybe)
  (when (memq major-mode '(nxml-mode web-mode))
    (save-excursion
      (forward-line -1)
      (es-aai-indent-line-maybe))))

(defun es-aai-correct-position-this ()
  "Go back to indentation if point is before indentation."
  (let ((indentation-beginning (es-indentation-end-pos)))
    (when (< (point) indentation-beginning)
      (goto-char indentation-beginning))))

(defun es-aai-before-change-function (&rest ignore)
  "Change tracking."
  (setq es-aai--change-flag t))

(defun* es-aai-post-command-hook ()
  "Correct the cursor, and possibly indent."
  (unless es-aai-mode
    (return-from es-aai-post-command-hook))
  (let* (( last-input-structural
           (member last-input-event
                   (mapcar 'string-to-char
                           (list "(" ")" "[" "]" "{" "}" "," ";" " "))))
         ( first-keystroke
           (and (eq this-command 'self-insert-command)
                (or last-input-structural
                    (not (eq last-command 'self-insert-command))))))
    ;; Correct position
    (when (and (es-neither (region-active-p)
                           (bound-and-true-p cua--rectangle)
                           (bound-and-true-p multiple-cursors-mode))
               (> (es-indentation-end-pos) (point)))
      (cond ( (memq this-command '(backward-char left-char))
              (forward-line -1)
              (goto-char (line-end-position)))
            ( (memq this-command
                    '(forward-char right-char
                      previous-line next-line))
              (back-to-indentation))))
    ;; It won't indent if corrected
    (when (and es-aai-after-change-indentation
               es-aai--change-flag
               (buffer-modified-p)
               (or first-keystroke
                   (not (memq this-command
                              '(save-buffer
                                delete-horizontal-space
                                undo
                                undo-tree-undo
                                undo-tree-redo
                                quoted-insert
                                backward-paragraph
                                self-insert-command))))
               (not (region-active-p)))
      (funcall es-aai-indent-function))
    (setq es-aai--change-flag nil)))

(defun es-aai--major-mode-setup ()
  "Optimizations for speicfic modes"
  (when (memq major-mode
              '(lisp-interaction-mode
                common-lisp-mode
                emacs-lisp-mode))
    (set (make-local-variable 'es-aai-indent-function)
         'es-aai-indent-defun)))

(defun es-aai--minor-mode-setup ()
  "Change interacting minor modes."
  (eval-after-load "multiple-cursors-core"
    '(pushnew 'es-aai-mode mc/unsupported-minor-modes))
  (eval-after-load "paredit"
    '(es-define-keys es-aai-mode-map
      [remap paredit-forward-delete] 'es-aai-delete-char
      [remap paredit-backward-delete] 'es-aai-backspace))
  (eval-after-load "cua-base"
    '(define-key cua--region-keymap [remap delete-char]
      (lambda ()
        (interactive)
        (if es-aai-mode
            (es-aai-delete-char)
            (cua-delete-region)))))
  (eval-after-load "eldoc"
    '(eldoc-add-command 'es-aai-indented-yank)))

(defun es-aai--init ()
  (pushnew 'es-aai-before-change-function before-change-functions)
  (add-hook 'post-command-hook 'es-aai-post-command-hook t t)
  (when cua-mode
    (es-define-keys es-aai-mode-map
      (kbd "C-v") 'es-aai-indented-yank))
  (es-define-keys es-aai-mode-map
    [mouse-2] 'es-aai-mouse-yank
    [remap yank] 'es-aai-indented-yank
    [remap newline] 'es-aai-newline-and-indent
    [remap open-line] 'es-aai-open-line
    [remap delete-char] 'es-aai-delete-char
    [remap forward-delete] 'es-aai-delete-char
    [remap backward-delete-char-untabify] 'es-aai-backspace
    [remap backward-delete-char] 'es-aai-backspace
    )
  (es-aai--minor-mode-setup)
  (es-aai--major-mode-setup))

(define-minor-mode es-aai-mode
    "Automatic automatic indentation.
Works pretty well for lisp out of the box.
Other modes might need some tweaking to set up:
If you trust the mode's automatic indentation completely, you can add to it's
init hook:

\(set \(make-local-variable 'es-aai-indent-function\)
     'es-aai-indent-defun\)

or

\(set \(make-local-variable 'es-aai-indent-function\)
     'es-aai-indent-forward\)

depending on whether the language has small and clearly
identifiable functions, that `beginning-of-defun' and
`end-of-defun' can find.

If on the other hand you don't trust the mode at all, but like
the cursor correction and delete-char behaviour,

you can add

\(set \(make-local-variable
      'es-aai-after-change-indentation\) t\)

if the mode indents pretty in all but a few cases, you can change the
`es-aai-indentable-line-p-function'. This is what I have in my php mode setup:

\(set \(make-local-variable
      'es-aai-indentable-line-p-function\)
     \(lambda \(\)
       \(not \(or \(es-line-matches-p \"EOD\"\)
                \(es-line-matches-p \"EOT\"\)\)\)\)\)"
  nil " aai" (make-sparse-keymap)
  (if es-aai-mode
      (es-aai--init)))

(defalias 'es-aa-indent-mode 'es-aai-mode)
(defvaralias 'es-aa-indent-mode 'es-aai-mode)

(provide 'es-lib-aa-indent)