#es-lib
A collecton of emacs utilities. Here are some highlights:

#### Files:

* **es-lib-move-text.el:**
  Functions for shifting current line or region in four directions
* **es-lib-duplicate.el:**
  Functions duplicating the current region
* **es-lib-total-line.el:**
  Functions for comfortably moving with folded lines
* **es-lib-number-at-point:**
  Functions for manipulating the number at point.

#### Functions:
* **es-ack-replace-symbol:**
  A refactoring tool, with help of which this library was assembled

## Index:
_Auto-generated before each commit. Total items in the library: 95_

### Commands:

* es-aai-backspace

```
Like `backward-delete-char', but removes the resulting gap when point is at EOL.
```

* es-aai-delete-char

```
Like `delete-char', but deletes indentation, if point is at it, or before it.
```

* es-aai-indented-yank
* es-aai-mouse-yank
* es-aai-mouse-yank-dont-indent
* es-aai-newline-and-indent
* es-aai-open-line

```
Open line, and indent the following.
```

* es-ack-pin-folder

```
Set ack root directory for one buffer only.
Ack won't prompt for a directory name in that buffer.
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
A simple version of expand-region for c-like languages.
Marks the symbol on first call, then marks the statement.
```

* es-comment-dwim
* es-decrease-number-at-point

```
See documentation for `es-increase-number-at-point'.
```

* es-delete-duplicate-lines
* es-duplicate-line

```
Duplicate current line.
```

* es-duplicate-line-or-region
* es-duplicate-region

```
Duplicate the active region.
```

* es-find-function-bound-to
* es-fixup-whitespace

```
Fixup white space between objects around point.
Leave one space or none, according to the context.

An improvment over the built-in fixup-whitespace.
You might want to do (defalias 'fixup-whitespace 'es-fixup-whitespace)
```

* es-highlighter

```
Like `highlight-symbol-at-point', but will also (un)highlight a phrase if the region is active.
```

* es-ido-like-helm

```
Choose from a concatenated list of buffers and recent files.
```

* es-increase-number-at-point

```
Increases the digit at point.
The increment some power of 10, depending on the positon of the cursor. If there
is no number at point, will try to increment the previous number on the same
line.
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
* es-move-text-down

```
Move region or the current line down.
```

* es-move-text-left

```
Move region or the current line left.
```

* es-move-text-right

```
Move region or the current line right.
```

* es-move-text-up

```
Move region or the current line up.
```

* es-new-empty-buffer
* es-push-line

```
beginning-of-line + open line.
```

* es-query-replace-symbol-at-point
* es-total-line-beginning

```
Interactive version of `es-total-line-beginning-position'.
```

* es-total-line-end

```
Interactive version of `es-total-line-end-position'.
```


### Non-interactive:

* es-aai--indent-region

```
Indent region lines where `es-aai-indentable-line-p-function' returns non-nil.
```

* es-aai--init
* es-aai--major-mode-setup

```
Optimizations for speicfic modes
```

* es-aai--minor-mode-setup

```
Change interacting minor modes.
```

* es-aai-before-change-function

```
Change tracking.
```

* es-aai-correct-position-this

```
Go back to indentation if point is before indentation.
```

* es-aai-indent-defun

```
Indents current defun, if it is smaller than `es-aai-indent-limit'.
Otherwise call `es-aai-indent-forward'.
```

* es-aai-indent-forward

```
Indent current line, and (1- `es-aai-indent-limit') lines afterwards.
```

* es-aai-indent-line-maybe

```
(indent-according-to-mode) when `es-aai-indentable-line-p-function' returns non-nil.
All indentation happends through this function.
```

* es-aai-post-command-hook

```
First key stroke tracking, cursor correction
```

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
* es-comp

```
Same as clojure's (comp).
```

* es-complement

```
Same as clojure's (complement).
```

* es-constantly

```
Same as clojure's (constantly).
```

* es-current-character-indentation

```
Like (current-indentation), but counts tabs as single characters.
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

* es-disable-keys
* es-find-duplicates

```
Multiple duplicates will be listed muliple times.
The "originals" won't be included.
```

* es-flip

```
Create a function with FUNC's arguments reversed.
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
Check whether the line contains a multiline folding.
```

* es-line-matches-p
* es-line-visible-p
* es-mapbuffer

```
Perform FUNCTION inside a 'with-current-buffer' for each member of BUFFER-LIST.
```

* es-mark-symbol-at-point
* es-mode-keymap
* es-next-printable-character-pos
* es-number-at-point
* es-point-between-pairs-p
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
Return t if STRING begins with BEGINNING.
```

* es-string-remove-properties
* es-toggle-true-false-maybe
* es-total-forward-line
* es-total-line-beginning-position

```
Kind of like (min (beginning-of-line) (beginning-of-visual-line)).
```

* es-total-line-end-position

```
Kind of like (max (end-of-line) (end-of-visual-line)).
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
 mvi-current-image-file nil)
```

* es-neither
* es-silence-messages
* es-while-point-moving