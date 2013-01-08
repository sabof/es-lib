(require 'cl)
(require 'es-lib-lexical)

(defmacro es-silence-messages (&rest body)
  `(flet ((message (&rest ignore)))
     ,@body))

(defmacro es-while-point-moving (&rest rest)
  (let ((old-point (gensym)))
    `(let (,old-point)
       (while (not (equal (point) ,old-point))
         (setq ,old-point (point))
         ,@rest))))

(defun* es-ido-completing-read-alist (prompt alist &rest rest)
  "Each member can also be a string"
  (require 'ido)
  (setq alist (mapcar (lambda (it) (if (consp it) it (cons it it)))
                      alist))
  (let (( selection (apply 'ido-completing-read prompt
                           (mapcar 'car alist) rest)))
    (when selection
      (cdr (find selection alist :key 'car :test 'equal)))))

(defun es-total-line-end-position (&optional pos)
  "Kind of like
 (max (end-of-line) (end-of-visual-line))"
  (save-excursion
    (when pos (goto-char pos))
    (es-while-point-moving
     (end-of-visual-line)
     (end-of-line))
    (point)))

(defun es-total-line-beginning-position (&optional pos)
  "Kind of like
 (min (beginning-of-line) (beginning-of-visual-line))"
  (save-excursion
    (when pos (goto-char pos))
    (es-while-point-moving
     (beginning-of-line)
     (beginning-of-visual-line))
    (point)))

(defun es-total-forward-line (arg)
  (cond
    ( (plusp arg)
      (dotimes (ignore arg)
        (goto-char (es-total-line-end-position))
        (forward-char)))
    ( t
      (goto-char (es-total-line-beginning-position))
      (dotimes (ignore (* -1 arg))
        (backward-char)
        (goto-char (es-total-line-beginning-position))
        ))))

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

(defun es-string-begins-with-p (string beginning)
  "Return t if and only if string begins with BEGINNING"
  (string-match-p (concat "^" (regexp-quote beginning)) string))

(defun es-string-remove-properties (string)
  (with-temp-buffer
    (insert string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun es-set-region (point mark)
  (push-mark mark)
  (goto-char point)
  (activate-mark)
  (setq deactivate-mark nil))

(defun es-line-matches-p (regexp)
  (string-match-p
   regexp
   (buffer-substring
    (line-beginning-position)
    (line-end-position))))

(defun es-indentation-end-pos (&optional position)
  (save-excursion
    (when position (goto-char position))
    (+ (current-indentation) (line-beginning-position))))

(defun es-line-empty-p ()
  (es-line-matches-p "^[ 	]*$"))

(defun es-line-visible-p ()
  (not (es-line-empty-p)))

(defun es-replace-regexp-prog (regexp replacement &optional from to)
  "By default acts on the whole buffer."
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
  "By default acts on the whole buffer."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (and from to)
          (narrow-to-region from to))
        (while (search-forward original nil t)
          (replace-match replacement t nil))))))

(defun es-find-function-bound-to (key-sequence)
  (interactive "kFind function bound to: ")
  (let ((symbol (key-binding key-sequence)))
    (if (fboundp symbol)
        (find-function symbol)
        (message "Key sequence unbound"))))

(defun es-add-at-eol (thing)
  (save-excursion
    (if (es-line-empty-p)
        (progn
          (next-line)
          (goto-char (es-last-character-pos))
          (insert thing))
        (progn
          (goto-char (es-last-character-pos))
          (insert thing)))))

(defun es-add-semicolon-at-eol ()
  (interactive)
  (es-add-at-eol ";")
  (when (fboundp 'fai-test-and-indent)
    (fai-test-and-indent)))

(defun es-add-comma-at-eol ()
  (interactive)
  (es-add-at-eol ","))

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
        ( t (beginning-of-line)
            (newline)
            (backward-char)
            (when (fboundp 'fai-test-and-indent)
              (fai-test-and-indent)))))

(defun es-jump-line ()
  "end-of-line + newline."
  (interactive)
  ;; (end-of-line)
  (goto-char (es-total-line-end-position))
  (funcall (or (key-binding (kbd "<return>"))
               (key-binding (kbd "\r"))
               (key-binding (kbd "RET"))
               'newline)))

(defun es-new-empty-buffer ()
  (interactive)
  (if (or t (> (length (window-list)) 1))
      (switch-to-buffer (generate-new-buffer "untitled"))
      (pop-to-buffer (generate-new-buffer "untitled")))
  (lisp-interaction-mode))

(defun* es-define-keys (keymap &rest bindings)
  "Returns the keymap in the end."
  (while bindings
    (define-key keymap (pop bindings) (pop bindings)))
  keymap)
(put 'es-define-keys 'common-lisp-indent-function
     '(4 &body))

(defun es-active-region-string ()
  (when (region-active-p)
    (buffer-substring
     (region-beginning)
     (region-end))))

(defun es-highlighter ()
  "Like (highlight-symbol-at-point), but will also (un)highlight
a phrase if the region is active."
  (interactive)
  (require 'highlight-symbol)
  (require 'hi-lock)
  (if (region-active-p)
      (let* ((phrase (regexp-quote (buffer-substring (point) (mark))))
             ;; hi-lock-interactive-patterns format:
             ;; (regexp (0 'face t))
             (pattern (find-if (lambda (element)
                                 (equal (first element) phrase))
                               hi-lock-interactive-patterns)))
        (if pattern
            (hi-lock-unface-buffer phrase)
            (let ((color (nth highlight-symbol-color-index
                              highlight-symbol-colors)))
              (if color ;; wrap
                  (incf highlight-symbol-color-index)
                  (setq highlight-symbol-color-index 1
                        color (car highlight-symbol-colors)))
              (setq color `((background-color . ,color)
                            (foreground-color . "black")))
              (hi-lock-set-pattern phrase color))))
      (highlight-symbol-at-point)))

(defun es-mouse-yank-replace-symbol (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (es-mark-symbol-at-point)
    (delete-region (point) (mark))
    (yank)))

(defun es-c-expand-region ()
  "A simple version of expand-region for c-like languages. Marks the symbol on
first call, then marks the statement."
  (interactive)
  (flet (( post-scriptum ()
           (when (and (equal (point) (line-end-position))
                      (not (equal (point) (point-max))))
             (forward-char))
           (exchange-point-and-mark))
         ( mark-statement-internal ()
           (back-to-indentation)
           (set-mark (point))
           (ignore-errors
             (while (equal (line-number-at-pos (point))
                           (line-number-at-pos (mark)))
               (forward-sexp)))
           (when (equal (char-after (point))
                        (aref ";" 0))
             (forward-char))
           (post-scriptum))
         ( mark-colon-internal ()
           (back-to-indentation)
           (set-mark (point))
           (re-search-forward ";")
           (when (equal (char-after (point))
                        (aref ";" 0))
             (forward-char))
           (post-scriptum))
         ( select-line-internal ()
           (back-to-indentation)
           (set-mark (point))
           (let (( next-colon
                   (save-excursion
                     (when (search-forward ";" nil t)
                       (point)))))
             (goto-char
              (if next-colon
                  (min next-colon (line-end-position))
                  (line-end-position))))
           (post-scriptum)))
    (cond ( (not (eq last-command this-command))
            (es-mark-symbol-at-point))
          ( (member (char-to-string (char-before (line-end-position)))
                    (list "{" "(" "["))
            (mark-statement-internal))
          ( (equal (char-before
                    (line-end-position))
                   ?: )
            (mark-colon-internal))
          ( t (select-line-internal)))))

(defun es-duplicate-line ()
  (let* (( pnt (point))
         ( start (es-total-line-beginning-position))
         ( end (es-total-line-end-position))
         ( copy-store (buffer-substring start end)))
    (goto-char end)
    (newline)
    (insert copy-store)
    (goto-char (+ 1 end (- pnt start)))))

(defun es-duplicate-region ()
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

(defun es-duplicate-line-or-region ()
  "Or region"
  (interactive)
  (if (region-active-p)
      (es-duplicate-region)
      (es-duplicate-line)))

(defun es-comment-dwim ()
  (interactive)
  (cond ( (region-active-p)
          (comment-or-uncomment-region (region-beginning) (region-end)))
        ( (es-line-empty-p)
          (cond ( (memq major-mode
                        '(lisp-mode lisp-interaction-mode emacs-lisp-mode))
                  (if (zerop (current-column))
                      (insert ";;; ")
                      (insert ";; ")))
                ( (member major-mode
                          '(php-mode c-mode
                            js2-mode js-mode))
                  (insert "// "))
                ( (eq major-mode 'css-mode)
                  (insert "/*  */")
                  (forward-char -3))
                ( t (insert comment-start)
                    (save-excursion (insert comment-end)))))
        ( t (comment-or-uncomment-region
             (line-beginning-position)
             (line-end-position))))
  (indent-according-to-mode))

(defun* es-ido-like-helm ()
  (interactive)
  (when (window-dedicated-p)
    (message "This is a dedicated window")
    (return-from es-ido-like-helm))
  (let* (( f:parent-dir
           (lambda (name)
             (file-name-nondirectory
              (directory-file-name
               (file-name-directory name)))))
         ( f:make-recentf-map
           (lambda (item)
             (cons (propertize
                    (concat (file-name-nondirectory item)
                            "<" (funcall f:parent-dir item) ">")
                    'face 'font-lock-keyword-face)
                   item)))
         ( buffer-list (buffer-name-list))
         ( recentf-map (mapcar f:make-recentf-map recentf-list))
         ( merged-list (append buffer-list recentf-map))
         ( no-duplicates
           (remove-duplicates
            merged-list
            :key (lambda (thing)
                   (if (stringp thing)
                       (or (buffer-file-name (get-buffer thing))
                           (symbol-name (gensym)))
                       (cdr thing)))
            :test 'equal
            :from-end t))
         ( junk-less
           (remove-if
            (es-back-curry 'member (list (buffer-name) "Map_Sym.txt"))
            no-duplicates))
         ( file
           (es-ido-completing-read-alist
            "Choose existing: " junk-less nil t)))
    (when file
      (if (member file buffer-list)
          (switch-to-buffer file)
          (find-file file)))))

(defun es-find-duplicates (list)
  "Multiple duplicates will be listed muliple times.
The \"originals\" won't be included."
  (let ((singles (remove-duplicates list :test 'equal))
        (clone (copy-list list)))
    (while singles
      (setq clone (remove* (pop singles) clone :test 'equal :count 1)))
    clone))

(defun es-delete-duplicate-lines (&optional beg end)
  (interactive)
  (setq beg (or beg
                (if (region-active-p)
                    (region-beginning))
                (point-min))
        end (or end
                (if (region-active-p)
                    (region-end))
                (point-max)))
  (let ((lines (split-string (buffer-substring beg end) "\n")))
    (delete-region beg end)
    (insert
     (mapconcat #'identity (delete-dups lines) "\n"))))

(defun es-next-printable-character-pos (&optional position)
  (flet ((char-after-or-nil ()
           (if (characterp (char-after))
               (char-to-string (char-after)))))
    (save-excursion
      (when position (goto-char position))
      (loop while (member (char-after-or-nil)
                          '(" " "	" "\n"))
            do (forward-char)
            finally (return (char-after-or-nil))))))

(defun es-mark-symbol-at-point ()
  (es-silence-messages
   (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
       (goto-char (match-end 0))
       (unless (memq (char-before) '(?\) ?\"))
         (forward-sexp)))
   (mark-sexp -1)
   (exchange-point-and-mark)
   (when (equal (char-after) ?\')
     (forward-char))))

(defun es-kill-dead-shelss ()
  (mapc 'kill-buffer-dont-ask
        (remove-if-not
         (lambda (buf)
           (and (eq (es-buffer-mode buf) 'shell-mode)
                (not (buffer-process buf))))
         (buffer-list))))

(defun* es-manage-unsaved-buffers()
  "Similar to what happends when emacs is about to quit."
  (interactive)
  (save-excursion
    (save-window-excursion
      (mapc ( lambda (buf)
              (switch-to-buffer buf)
              (case (read-char
                     "cNext(n) Save(s) Save All(!) Edit(e) Kill(k)? ")
                ( ?!
                  (dolist (buf (unsaved-buffer-list))
                    (with-current-buffer buf
                      (save-buffer)))
                  (return-from es-manage-unsaved-buffers))
                ( ?s (save-buffer))
                ( ?k (kill-buffer-dont-ask))
                ( ?e (recursive-edit))))
            ( or (unsaved-buffer-list)
                 (progn
                   (message "All buffers are saved")
                   (return-from es-manage-unsaved-buffers))))
      (message "Done"))))


(defun es-query-replace-symbol-at-point ()
  (interactive)
  (let* (( original
           (if (region-active-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end))
               (symbol-name
                (symbol-at-point))))
         ( replace-what
           (if (region-active-p)
               (regexp-quote original)
               (concat "\\_<"
                       (regexp-quote original)
                       "\\_>")))
         ( replacement
           (read-from-minibuffer
            (format "Replace \"%s\" with: " original)
            original)))
    (save-excursion
      (query-replace-regexp
       replace-what
       replacement
       nil (line-beginning-position)
       (point-max)))
    (when (save-excursion
            (re-search-backward
             replace-what
             nil t))
      (save-excursion
        (query-replace-regexp
         replace-what
         replacement
         nil (point-min) (line-beginning-position))))))

(defun es-last-character-pos ()
  (save-match-data
    (save-excursion
      (end-of-line)
      (if (re-search-backward
           "[^ \t]" (line-beginning-position) t)
          (progn (forward-char)
                 (point))
          (line-beginning-position)))))

(defun es-line-folded-p ()
  "Checks whether the line contains a multiline folding"
  (not (equal (list (line-beginning-position)
                    (line-end-position))
              (list (es-total-line-beginning-position)
                    (es-total-line-end-position)))))

(defun es-mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

(defun es-number-at-point ()
  (when (looking-at "[[:digit:]-]+")
    (save-excursion
      (while (looking-at "[[:digit:]-]+")
        (backward-char))
      (list (match-string-no-properties 0)
            (match-beginning 0)
            (match-end 0)))))

(defun es-toggle-true-false-maybe ()
  (save-excursion
    (flet (( replace (new)
             (es-mark-symbol-at-point)
             (delete-region (point) (mark))
             (insert new)
             t))
      (cond ( (eq (symbol-at-point) 'true)
              (replace "false"))
            ( (eq (symbol-at-point) 'false)
              (replace "true"))
            ( (eq (symbol-at-point) 't)
              (replace "nil"))
            ( (equal (word-at-point) "nil")
              (replace "t"))
            ( t nil)))))

(defun es-goto-previous-non-blank-line ()
  (save-match-data
    (beginning-of-line)
    (re-search-backward "[^ \n\t]")
    (beginning-of-line)))

(defun es-current-character-indentation ()
  "Like (current-indentation), but counts tabs as single characters"
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun* es-ack-replace-symbol
    (from-symbol-or-string
     to-symbol-or-string
     &key
     directory
     auto-save
     finish-func
     silent)
  "Repalace symbol at point, or region contents in multiple
files."
  (interactive (list nil nil))
  (require 'ack-and-a-half)
  (require 'wgrep)
  (require 'wgrep-ack)
  (let ((ack-and-a-half-arguments (list "-Q"))
        was-symbol)
    ;; Argument processing
    (if (called-interactively-p 'any)
        (progn (setq from-symbol-or-string
                     (or (es-active-region-string)
                         (when (symbol-at-point)
                           (setq was-symbol t)
                           (symbol-name (symbol-at-point)))
                         (let (sym)
                           (when (setq sym (read-string
                                            "Ack Replace which symbol: "))
                             (setq was-symbol t)
                             sym))
                         (return-from es-ack-replace-symbol)))
               (setq to-symbol-or-string
                     (read-string
                      (format
                       "Ack Replace %s with: "
                       from-symbol-or-string)
                      from-symbol-or-string))
               (setq directory (ack-and-a-half-read-dir)))
        (progn (when (symbolp from-symbol-or-string)
                 (setq was-symbol t)
                 (setq from-symbol-or-string
                       (symbol-name from-symbol-or-string)))
               (when (symbolp to-symbol-or-string)
                 (setq to-symbol-or-string
                       (symbol-name to-symbol-or-string)))))
    (and (buffer-modified-p)
         (y-or-n-p "Save current buffer?")
         (save-buffer))
    (with-current-buffer
        (ack-and-a-half from-symbol-or-string nil directory)
      (set (make-local-variable 'compilation-finish-functions)
           (list `(lambda (buffer status)
                    (with-current-buffer buffer
                      (compilation-next-error 1)
                      (wgrep-change-to-wgrep-mode)
                      (es-replace-regexp-prog
                       ,(if was-symbol
                            (format
                             "\\_<%s\\_>"
                             (regexp-quote
                              from-symbol-or-string))
                            (regexp-quote
                             from-symbol-or-string))
                       ,to-symbol-or-string
                       (point)
                       (point-max))
                      (when ,auto-save
                        (let ((wgrep-auto-save-buffer t))
                          (wgrep-finish-edit)
                          (quit-window)))
                      (funcall (or ,finish-func 'ignore)))))))))

(defun* es-change-number-at-point (&optional decrease)
  (let ((number (es-number-at-point)))
    (if (not number)
        (progn
          (save-excursion
            (when (re-search-backward "[0-9]" (line-beginning-position) t)
              (es-change-number-at-point decrease)))
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

(defun es-ack-pin-folder (folder)
  "Set ack root directory for one buffer only"
  (interactive
   (list (read-directory-name "Directory for ack: ")))
  (set (make-local-variable
        'ack-and-a-half-root-directory-functions)
       (list `(lambda () ,folder)))
  (set (make-local-variable
        'ack-and-a-half-prompt-for-directory) nil)
  (message "Ack directory set to: %s" folder))

(defun es-increase-number-at-point ()
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es-change-number-at-point)))

(defun es-decrease-number-at-point ()
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es-change-number-at-point t)))

(defun es-windows-with-buffer (buffer)
  "In all frames"
  (remove-if-not
   (lambda (window)
     (eq (window-buffer window) buffer))
   (loop for frame in (frame-list)
         append (window-list frame))))

(defun es-random-member (list)
  (nth (random (length list)) list))

(defun es-pop-to-buffer-vertically (buf)
  (let ((split-height-threshold 0)
        (split-width-threshold 0))
    (pop-to-buffer buf)))

(provide 'es-lib)