#es-lib
A collecton of emacs utilities, and basis for several of my packages. Here are some highlights:

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

_Auto-generated before each commit. Total items in the library: 42_

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
