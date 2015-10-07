#es-lib
A collection of emacs utilities, and basis for several of my packages. Here are some highlights:

#### Packages:

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

_Auto-generated before each commit. Total items in the library: 100_

#### Table of contents:

* [es-lib-buffer-local-set-key](#es-lib-buffer-local-set-key)
* [es-lib-core-functions](#es-lib-core-functions)
* [es-lib-core-macros](#es-lib-core-macros)
* [es-lib-duplicate](#es-lib-duplicate)
* [es-lib-lexical](#es-lib-lexical)
* [es-lib-number-at-point](#es-lib-number-at-point)
* [es-lib-text-navigate](#es-lib-text-navigate)
* [es-lib-total-line](#es-lib-total-line)



## es-lib-buffer-local-set-key


#### Defvars:

* es-buffer-local-mode

#### Non-interactive:

* es-buffer-local-set-key
* es-buffer-local-set-keys

## es-lib-core-functions


#### Defvars:

* es-figlet-font-history
* es-figlet-phrase-history

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
A simplee version of expand-region for c-like languages.
Marks the symbol on first call, then marks the statement.
```

* es-comment-dwim
* es-delete-duplicate-lines
* es-figlet-insert

```
Insert a figlet-formatted phrase at point:
 _____ _       _      _
|  ___(_) __ _| | ___| |_
| |_  | |/ _` | |/ _ \ __|
|  _| | | (_| | |  __/ |_
|_|   |_|\__, |_|\___|\__|
         |___/
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

* es-jump-line

```
end-of-line + newline.
```

* es-kill-buffer-dont-ask
* es-manage-unsaved-buffers

```
Similar to what happends when emacs is about to quit.
```

* es-mouse-copy-symbol
* es-mouse-yank-replace-symbol
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

```
Will omit special and tag buffers.
```

* es-buffers-where-local-variable-is
* es-buffers-with-mode
* es-color-emacs-color-to-hex
* es-color-hex-to-list
* es-color-list-to-hex
* es-color-normalize-hex
* es-color-random-hex
* es-define-keys

```
Syntax example:
(es-define-keys fundamental-mode-map
  (kbd "h") 'backward-char
  (kbd "l") 'forward-char)
 Returns the keymap in the end.

(fn KEYMAP &rest BINDINGS)
```

* es-disable-buffer-scrolling
* es-disable-keys
* es-figlet-fonts
* es-find-duplicates

```
Multiple duplicates will be listed muliple times.
The "originals" won't be included.
```

* es-full-window-list

```
Return all windows from all frames
```

* es-ido-completing-read-alist

```
Each member can also be a string

(fn PROMPT ALIST &rest REST)
```

* es-kill-dead-shells
* es-mapbuffer

```
Perform FUNCTION inside a buffer with each member of BUFFER-LIST as current.
FUNCTION does not accept arguments
```

* es-mode-keymap
* es-next-match-pos
* es-next-visible-character-at-pos
* es-pop-to-buffer-vertically
* es-preserve-overlay
* es-random-member
* es-realize-overlay
* es-replace-in-string-multiple

```
For each member of ALIST, replace all occurances of car with cdr.
car is a literal string, not a regular expression.
```

* es-replace-prog

```
By default acts on the whole buffer.
```

* es-replace-regexp-prog

```
By default acts on the whole buffer.
```

* es-reset-feature
* es-restore-overlay
* es-string-remove-properties
* es-toggle-true-false-maybe
* es-unsaved-buffer-list
* es-var-documentation

```
Get variable documentation, or nil if there isn't one.
```

* es-virtualize-overlay
* es-windows-with-buffer

```
In all frames.
```


## es-lib-core-macros


#### Macros:

* es-after

```
`eval-after-load' MODE evaluate BODY.
```

* es-back-pop
* es-back-push
* es-define-buffer-local-vars

```
Syntax example:
(es-define-buffer-local-vars
 mvi-current-image-file nil)
```

* es-neither
* es-opts
* es-preserve-functions

```
A helper for loading packages.
Example of usage:

(es-preserve-functions
  (default-function-i-like1
    default-function-i-like2)
(require 'some-package-that-redefines-them-at-top-level)
)

This is a hack, and in no way it excuses package-authors who do that.
They should provide initialization functions that execute the redefinitions.

(fn (&rest FUNCS) &rest BODY)
```

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

* es-goto-line-prog

```
Like goto-line, but simplified for programmatic use.
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
