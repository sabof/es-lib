#es-lib
A collecton of emacs utilities.
## Index:
_(auto-generated before each commit)_

### Commands:

* es-ack-pin-folder

```
Set ack root directory for one buffer only. Ack won't prompt for a directory
name in that buffer.
```

* es-ack-replace-symbol

```
Repalace symbol at point, or region contents in multiple
files.

(fn FROM-SYMBOL-OR-STRING TO-SYMBOL-OR-STRING &key DIRECTORY AUTO-SAVE FINISH-FUNC SILENT)
```

* es-add-comma-at-eol
* es-add-semicolon-at-eol
* es-c-expand-region

```
A simple version of expand-region for c-like languages. Marks the symbol on
first call, then marks the statement.
```

* es-comment-dwim
* es-decrease-number-at-point

```
See documentation for es-increase-number-at-point.
```

* es-delete-duplicate-lines
* es-duplicate-line-or-region
* es-find-function-bound-to
* es-highlighter

```
Like (highlight-symbol-at-point), but will also (un)highlight
a phrase if the region is active.
```

* es-ido-like-helm

```
Choose from a concatenated list of buffers and recent files.
```

* es-increase-number-at-point

```
Increases the digit at point. The increment some power of 10, depending on
the positon of the cursor. If there is no number at point, will try to
increment the previous number on the same line.
```

* es-jump-line

```
end-of-line + newline.
```

* es-kill-buffer-dont-ask
* es-make-timer-buffer

```
Accepts a time-limit in minutes.
```

* es-manage-unsaved-buffers

```
Similar to what happends when emacs is about to quit.
```

* es-mouse-yank-replace-symbol
* es-new-empty-buffer
* es-push-line

```
beginning-of-line + open line.
```

* es-query-replace-symbol-at-point

### Non-interactive:

* es-active-region-string
* es-add-at-eol

```
Insert THING at end of line.
If the line is empty, insert at the end of next line.
```

* es-back-curry

```
Like (apply-partially), but adds arguments to the end.
```

* es-buffer-mode
* es-buffer-name-list
* es-buffers-where-local-variable-is
* es-buffers-with-mode
* es-change-number-at-point
* es-comp

```
Same as clojure's (comp)
```

* es-complement

```
Same as clojure's (complement)
```

* es-constantly

```
Same as clojure's (constantly)
```

* es-current-character-indentation

```
Like (current-indentation), but counts tabs as single characters
```

* es-define-keys

```
Syntax example:
(es-define-keys fundamental-mode-map
  (kbd "h") 'backward-char
  (kbd "l") 'forward-char)
 Returns the keymap in the end.

(fn KEYMAP &rest BINDINGS)
```

* es-duplicate-line
* es-duplicate-region
* es-find-duplicates

```
Multiple duplicates will be listed muliple times.
The "originals" won't be included.
```

* es-flip

```
Creates a function with FUNC's arguments reversed.
```

* es-goto-previous-non-blank-line
* es-ido-completing-read-alist

```
Each member can also be a string

(fn PROMPT ALIST &rest REST)
```

* es-indentation-end-pos
* es-kill-dead-shells
* es-line-empty-p
* es-line-folded-p

```
Checks whether the line contains a multiline folding
```

* es-line-matches-p
* es-line-visible-p
* es-mapbuffer

```
Perform FUNCTION inside a with-current-buffer for each member of
BUFFER-LIST.
```

* es-mark-symbol-at-point
* es-mode-keymap
* es-next-printable-character-pos
* es-number-at-point
* es-pop-to-buffer-vertically
* es-random-member
* es-replace-prog

```
By default acts on the whole buffer.
```

* es-replace-regexp-prog

```
By default acts on the whole buffer.
```

* es-set-region
* es-string-begins-with-p

```
Return t if and only if string begins with BEGINNING
```

* es-string-remove-properties
* es-toggle-true-false-maybe
* es-total-forward-line
* es-total-line-beginning-position

```
Kind of like
 (min (beginning-of-line) (beginning-of-visual-line))
```

* es-total-line-end-position

```
Kind of like
 (max (end-of-line) (end-of-visual-line))
```

* es-unsaved-buffer-list
* es-visible-end-of-line
* es-windows-with-buffer

```
In all frames.
```


### Macros:

* es-back-pop
* es-define-buffer-local-vars

```
Syntax example:
(es-define-buffer-local-vars
 mvi-current-image-file nil
 mvi-resize-timer nil
 mvi-is-mvi-buffer nil
 mvi-last-image nil
 mvi-buffer-tmp-file nil
 mvi-buffer-lock nil
 mvi-buffer-queue nil
 )
```

* es-neither
* es-silence-messages
* es-while-point-moving