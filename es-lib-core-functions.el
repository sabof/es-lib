(require 'cl)

(defun es-disable-keys (map &rest keylist)
  (dolist (key keylist)
    (define-key map key nil)))
(put 'es-disable-keys 'common-lisp-indent-function
     '(4 &body))

(defun es-kill-buffer-dont-ask (&optional buffer)
  (interactive)
  (when buffer (set-buffer buffer))
  (set-buffer-modified-p nil)
  (let (kill-buffer-query-functions)
    (kill-buffer)))

(defun es-buffer-name-list ()
  (remove-if (lambda (name)
               (some (es-back-curry 'string-match-p name)
                     (list "^ " "^tags$" "^TAGS$")))
             (mapcar 'buffer-name (buffer-list))))

(defun es-unsaved-buffer-list ()
  (save-window-excursion
    (loop for buf in (buffer-list)
          when (progn (switch-to-buffer buf)
                      (and buffer-file-name
                           (buffer-modified-p)))
          collect (buffer-name buf))))

(defun* es-ido-completing-read-alist (prompt alist &rest rest)
  "Each member can also be a string"
  (require 'ido)
  (setq alist (mapcar (lambda (it) (if (consp it) it (cons it it)))
                      alist))
  (let (( selection (apply 'ido-completing-read prompt
                           (mapcar 'car alist) rest)))
    (when selection
      (cdr (find selection alist :key 'car :test 'equal)))))

(defun es-buffer-mode (buffer-or-name)
  (with-current-buffer (get-buffer buffer-or-name)
    major-mode))

(defun es-mapbuffer (function buffer-list)
  "Perform FUNCTION inside a 'with-current-buffer' for each member of BUFFER-LIST."
  (mapcar (lambda (buf)
            (with-current-buffer buf
              (funcall function buf)))
          buffer-list))

(defun es-buffers-where-local-variable-is (var-sym value)
  (remove-if-not (lambda (buf)
                   (with-current-buffer buf
                     (equal (symbol-value var-sym)
                            value)))
                 (buffer-list)))

(defun es-string-begins-with-p (string beginning)
  "Return t if STRING begins with BEGINNING."
  (string-match-p (concat "^" (regexp-quote beginning)) string))

(defun es-string-remove-properties (string)
  (with-temp-buffer
    (insert string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun es-replace-regexp-prog (regexp replacement &optional from to)
  "By default acts on the whole buffer."
  (assert (or (es-neither from to) (and from to)))
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

;;;###autoload
(defun es-find-function-bound-to (key-sequence)
  (interactive "kFind function bound to: ")
  (let ((symbol (key-binding key-sequence)))
    (if (fboundp symbol)
        (find-function symbol)
        (message "Key sequence unbound"))))

(defun es-add-at-eol (thing)
  "Insert THING at end of line.
If the line is empty, insert at the end of next line."
  (save-excursion
    (if (es-line-empty-p)
        (progn
          (next-line)
          (goto-char (es-visible-end-of-line))
          (insert thing))
        (progn
          (goto-char (es-visible-end-of-line))
          (insert thing)))))

(defun es-add-semicolon-at-eol ()
  (interactive)
  (es-add-at-eol ";")
  (when (fboundp 'es-aai-indent-line-maybe)
    (es-aai-indent-line-maybe)))

(defun es-add-comma-at-eol ()
  (interactive)
  (es-add-at-eol ","))

(defun es-buffers-with-mode (mode)
  (remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (eq major-mode mode)))
   (buffer-list)))

;;;###autoload
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
          (call-interactively
           (key-binding (kbd "C-e")))
          (end-of-line)
          (call-interactively
           (or (key-binding (kbd "<return>"))
               (key-binding (kbd "\r"))
               (key-binding (kbd "RET"))
               'newline)))
        ( t (beginning-of-line)
            (newline)
            (backward-char)
            (when (fboundp 'es-aai-indent-line-maybe)
              (es-aai-indent-line-maybe)))))

;;;###autoload
(defun es-jump-line ()
  "end-of-line + newline."
  (interactive)
  (goto-char (es-total-line-end-position))
  (call-interactively
   (or (key-binding (kbd "<return>"))
       (key-binding (kbd "\r"))
       (key-binding (kbd "RET"))
       'newline)))

;;;###autoload
(defun es-new-empty-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (lisp-interaction-mode))

(defun* es-define-keys (keymap &rest bindings)
  "Syntax example:
\(es-define-keys fundamental-mode-map
  (kbd \"h\") 'backward-char
  (kbd \"l\") 'forward-char\)
 Returns the keymap in the end."
  (while bindings
    (define-key keymap (pop bindings) (pop bindings)))
  keymap)
(put 'es-define-keys 'common-lisp-indent-function
     '(4 &body))

;;;###autoload
(defun es-highlighter ()
  "Like `highlight-symbol-at-point', but will also (un)highlight a phrase if the region is active."
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

;;;###autoload
(defun es-mouse-yank-replace-symbol (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (es-mark-symbol-at-point)
    (delete-region (point) (mark))
    (deactivate-mark)
    (yank)))

(defun es-next-match-pos (regex)
  (save-excursion
    (when (re-search-forward regex nil t)
      (point))))

;;;###autoload
(defun es-c-expand-region ()
  "A simple\(?\) version of expand-region for c-like languages.
Marks the symbol on first call, then marks the statement."
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
           (let (next-opening-bracket next-colon)
             (back-to-indentation)
             (set-mark (point))
             (setq next-opening-bracket (es-next-match-pos "{")
                   next-colon (es-next-match-pos ";"))
             (cond ( (and next-opening-bracket next-colon)
                     (if (< next-opening-bracket next-colon)
                         (progn (goto-char next-opening-bracket)
                                (backward-char)
                                (forward-sexp))
                         (goto-char next-colon)))
                   ( next-opening-bracket
                     (progn (goto-char next-opening-bracket)
                            (backward-char)
                            (forward-sexp)))
                   ( t (goto-char next-colon)))
             (when (equal (char-after (point))
                          (aref ";" 0))
               (forward-char))
             (post-scriptum)))
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
          ( (member (char-before (es-visible-end-of-line))
                    '( ?: ?, ) )
            (mark-colon-internal))
          ( t (select-line-internal)))))

;;;###autoload
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

;;;###autoload
(defun* es-ido-like-helm ()
  "Choose from a concatenated list of buffers and recent files."
  (interactive)
  (require 'recentf)
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
         ( buffer-list (es-buffer-name-list))
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

;;;###autoload
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

(defun es-kill-dead-shells ()
  (mapc 'es-kill-buffer-dont-ask
        (remove-if-not
         (lambda (buf)
           (and (eq (es-buffer-mode buf) 'shell-mode)
                (not (buffer-process buf))))
         (buffer-list))))

;;;###autoload
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
                  (dolist (buf (es-unsaved-buffer-list))
                    (with-current-buffer buf
                      (save-buffer)))
                  (return-from es-manage-unsaved-buffers))
                ( ?s (save-buffer))
                ( ?k (es-kill-buffer-dont-ask))
                ( ?e (recursive-edit))))
            ( or (es-unsaved-buffer-list)
                 (progn
                   (message "All buffers are saved")
                   (return-from es-manage-unsaved-buffers))))
      (message "Done"))))

;;;###autoload
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

(defun es-mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

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

;;;###autoload
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
         (y-or-n-p "Save current buffer? ")
         (save-buffer))
    (ack-and-a-half from-symbol-or-string nil directory)
    (dolist (window (window-list))
      (when (eq (es-buffer-mode
                 (window-buffer window))
                'ack-and-a-half-mode)
        (select-window window)))
    (set (make-local-variable 'compilation-finish-functions)
         (list `(lambda (buffer status)
                  (with-current-buffer buffer
                    (ignore-errors
                      (compilation-next-error 1))
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
                    (funcall (or ,finish-func 'ignore))))))))

;;;###autoload
(defun es-ack-pin-folder (folder)
  "Set ack root directory for one buffer only.
Ack won't prompt for a directory name in that buffer."
  (interactive
   (list (read-directory-name "Directory for ack: ")))
  (set (make-local-variable
        'ack-and-a-half-root-directory-functions)
       (list `(lambda () ,folder)))
  (set (make-local-variable
        'ack-and-a-half-prompt-for-directory) nil)
  (message "Ack directory set to: %s" folder))

(defun es-windows-with-buffer (buffer)
  "In all frames."
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

(defun es-fixup-whitespace (&optional before after)
  "Fixup white space between objects around point.
Leave one space or none, according to the context.

An improvment over the built-in fixup-whitespace.
You might want to do \(defalias 'fixup-whitespace 'es-fixup-whitespace\)"
  (interactive "*")
  (delete-horizontal-space)
  (macrolet ((sp-member (arg)
               `(member ,arg SPairRaw)))
    (let* ((SPairRaw (list (or (char-before) before)
                           (or (char-after) after)))
           (SPair (mapcar (lambda (char)
                            (if (characterp char)
                                (char-to-string char)))
                          SPairRaw))
           (insert-space
            (block insert-space-b
              ;; All Langs
              (when (or (and (sp-member ?\") (in-string-p))
                        (equal SPairRaw '(  ?>  ?<  ))
                        (sp-member ?\s)
                        (sp-member ?\n)
                        ;; (eq '(   ?\(   ?\(   ) SPairRaw)
                        (member (first SPairRaw)
                                '(   ?\(   ))
                        (member (second SPairRaw)
                                '(   ?\)   ?\n   )))
                (return-from insert-space-b nil))
              ;; C Family
              (when (memq major-mode '(php-mode  c-mode  js2-mode  js-mode  css-mode))
                (when (equal (first SPairRaw) ?\;)
                  (return-from insert-space-b t))
                (when (or (equal (second SPairRaw) ?\;)
                          (equal SPairRaw '(?\} ?\}))
                          (and (in-string-p) (sp-member ?\'))
                          (member (second SPairRaw) '(?\n  ?\)   ?\(   ?,   ?:   nil)))
                  (return-from insert-space-b nil)))
              ;; Lisp Family
              (when (memq major-mode '(lisp-mode emacs-lisp-mode lisp-interaction-mode))
                (when (or (equal SPairRaw '(  ?'  ?\(  )))
                  (return-from insert-space-b nil))
                (when (or (equal SPairRaw '(  ?\)  ?\(  ))
                          (and (sp-member ?\)) (not (eq (char-before) (char-after))))
                          (and (sp-member ?\() (not (eq (char-before) (char-after)))))
                  (return-from insert-space-b t)))
              (when (memq major-mode '(nxml-mode php-mode web-mode))
                (when (or (sp-member ?<)
                          (sp-member ?>))
                  (return-from insert-space-b nil)))
              t)))
      (when insert-space
        (insert ?\s))
      insert-space)))

(defun es-reset-feature (feature)
  (when (featurep feature)
    (ignore-errors
      (unload-feature feature t)))
  (require feature))

(defun es-var-documentation (sym)
  "Get variable documentation, or nil if there isn't one."
  (ignore-errors
    (documentation-property
     sym 'variable-documentation t)))

(defun es-color-list-to-hex (color-list)
  (apply 'format "#%02X%02X%02X" color-list))

(defun es-color-normalize-hex (hex-string)
  (if (string-match-p "^#" hex-string)
      (upcase
       (if (= (length hex-string) 4)
           (apply 'concat "#"
                  (mapcar
                   (lambda (pair)
                     (make-string
                      2 (string-to-char
                         (substring
                          hex-string (car pair) (cdr pair)))))
                   '((1 . 2) (2 . 3) (3 . 4))))
           hex-string))
      hex-string))

(defun es-color-hex-to-list (hex-color)
  (let ((hex-color (es-color-normalize-hex hex-color)))
    (list (string-to-int (substring hex-color 1 3) 16)
          (string-to-int (substring hex-color 3 5) 16)
          (string-to-int (substring hex-color 5 7) 16))))

(defun es-color-emacs-color-to-hex (color)
  (let ((color-values (color-values color)))
    (apply 'format "#%02x%02x%02x"
           (mapcar (lambda (c) (lsh c -8))
                   color-values))))

(defun es-color-random-hex ()
  (es-color-list-to-hex (mapcar 'random (make-list 3 255))))

(defun es-disable-buffer-scrolling ()
  (es-buffer-local-set-keys
    [remap smooth-scroll-up] 'ignore
    [remap smooth-scroll-down] 'ignore
    [remap cua-scroll-up] 'ignore
    [remap cua-scroll-down] 'ignore
    [remap scroll-up-command] 'ignore
    [remap scroll-down-command] 'ignore
    (kbd "<down-mouse-1>") 'ignore
    (kbd "<drag-mouse-1>") 'ignore
    (kbd "<mouse-5>") 'ignore
    (kbd "<mouse-4>") 'ignore
    (kbd "<next>") 'ignore
    (kbd "<prior>") 'ignore)
  (add-hook 'post-command-hook (lambda () (set-window-start (selected-window) (point-min)))
            t t))

(provide 'es-lib-core-functions)