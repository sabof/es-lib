#es-lib
A collecton of emacs utilities, and basis for several of my packages. Here are some highlights:

#### Packages:

* **es-lib-move-text:**
  Functions for shifting current line or region in four directions
* **es-lib-duplicate:**
  Functions duplicating the current region
* **es-lib-total-line:**
  Functions for comfortably moving with folded lines
* **es-lib-number-at-point:**
  Functions for manipulating the number at point.

#### Functions:

* **es-ack-replace-symbol:**
  A refactoring tool, with help of which this library was assembled
* **es-ido-like-helm:**
  Choose from a concatenated list of buffers and recent files. I have it bound to `<menu>`.

# Index:

_Auto-generated before each commit. Total items in the library: 80_

#### Table of contents:

* [es-lib-core-functions](#es-lib-core-functions)
* [es-lib-core-macros](#es-lib-core-macros)
* [es-lib-duplicate](#es-lib-duplicate)
* [es-lib-lexical](#es-lib-lexical)
* [es-lib-number-at-point](#es-lib-number-at-point)
* [es-lib-text-navigate](#es-lib-text-navigate)
* [es-lib-total-line](#es-lib-total-line)



## es-lib-core-functions


#### Commands:

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
A simple(?) version of expand-region for c-like languages.
Marks the symbol on first call, then marks the statement.
```

* es-comment-dwim
* es-delete-duplicate-lines
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

* es-jump-line

```
end-of-line + newline.
```

* es-kill-buffer-dont-ask
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

#### Non-interactive:

* es-add-at-eol

```
Insert THING at end of line.
If the line is empty, insert at the end of next line.
```

* es-buffer-mode
* es-buffer-name-list
* es-buffers-where-local-variable-is
* es-buffers-with-mode
* es-color-hex-to-list
* es-color-list-to-hex
* es-color-normalize-hex
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

* es-ido-completing-read-alist

```
Each member can also be a string

(fn PROMPT ALIST &rest REST)
```

* es-kill-dead-shells
* es-mapbuffer

```
Perform FUNCTION inside a 'with-current-buffer' for each member of BUFFER-LIST.
```

* es-mode-keymap
* es-next-match-pos
* es-next-printable-character-pos
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

* es-reset-feature
* es-string-begins-with-p

```
Return t if STRING begins with BEGINNING.
```

* es-string-remove-properties
* es-toggle-true-false-maybe
* es-unsaved-buffer-list
* es-var-documentation

```
Get variable documentation, or nil if there isn't one.
```

* es-windows-with-buffer

```
In all frames.
```


## es-lib-core-macros


#### Macros:

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

## es-lib-duplicate


#### Commands:

* es-duplicate-line

```
Duplicate current line.
```

* es-duplicate-line-or-region
* es-duplicate-region

```
Duplicate the active region.
```


## es-lib-lexical


#### Commands:

* es-make-timer-buffer

```
Accepts a time-limit in minutes.
```


#### Non-interactive:

* es-back-curry

```
Like (apply-partially), but adds arguments to the end.
```

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

* es-flip

```
Create a function with FUNC's arguments reversed.
```


## es-lib-number-at-point


#### Commands:

* es-decrease-number-at-point

```
See documentation for `es-increase-number-at-point'.
```

* es-increase-number-at-point

```
Increases the digit at point.
The increment some power of 10, depending on the positon of the cursor. If there
is no number at point, will try to increment the previous number on the same
line.
```


#### Non-interactive:

* es-number-at-point

## es-lib-text-navigate


#### Non-interactive:

* es-active-region-string
* es-current-character-indentation

```
Like (current-indentation), but counts tabs as single characters.
```

* es-goto-previous-non-blank-line
* es-indentation-end-pos
* es-line-empty-p
* es-line-folded-p

```
Check whether the line contains a multiline folding.
```

* es-line-matches-p
* es-line-visible-p
* es-mark-symbol-at-point
* es-point-between-pairs-p
* es-set-region
* es-visible-end-of-line

## es-lib-total-line


#### Commands:

* es-total-line-beginning

```
Interactive version of `es-total-line-beginning-position'.
```

* es-total-line-end

```
Interactive version of `es-total-line-end-position'.
```


#### Non-interactive:

* es-total-forward-line
* es-total-line-beginning-position

```
Kind of like (min (beginning-of-line) (beginning-of-visual-line)).
```

* es-total-line-end-position

```
Kind of like (max (end-of-line) (end-of-visual-line)).
```