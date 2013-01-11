;;; Comment:
;; Functions for moving around a buffer, and getting information about the
;; surrounding text.

;;; Code:

(defun es-point-between-pairs-p ()
  (let ((result nil))
    (mapcar*
     (lambda (character-pair)
       (if (and (characterp (char-before))
                (characterp (char-after))
                (equal (char-to-string (char-before))
                       (car character-pair))
                (equal (char-to-string (char-after))
                       (cdr character-pair)))
           (setq result t)))
     '(("\"" . "\"")
       ("\'" . "\'")
       ("{" . "}")
       ("(" . ")")
       ("[" . "]")))
    result))

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

(defun es-active-region-string ()
  (when (region-active-p)
    (buffer-substring
     (region-beginning)
     (region-end))))

(defun es-goto-previous-non-blank-line ()
  (save-match-data
    (beginning-of-line)
    (re-search-backward "[^ \n\t]" nil t)
    (beginning-of-line)))

(defun es-current-character-indentation ()
  "Like (current-indentation), but counts tabs as single characters."
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun es-visible-end-of-line ()
  (save-match-data
    (save-excursion
      (end-of-line)
      (if (re-search-backward
           "[^ \t]" (line-beginning-position) t)
          (progn (forward-char)
                 (point))
          (line-beginning-position)))))

(defun es-line-folded-p ()
  "Check whether the line contains a multiline folding."
  (not (equal (list (line-beginning-position)
                    (line-end-position))
              (list (es-total-line-beginning-position)
                    (es-total-line-end-position)))))

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
    (+ (es-current-character-indentation)
       (line-beginning-position))))

(defun es-line-empty-p ()
  (es-line-matches-p "^[ 	]*$"))

(defun es-line-visible-p ()
  (not (es-line-empty-p)))

(provide 'es-lib-text-navigate)