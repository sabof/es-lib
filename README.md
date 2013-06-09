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

_Auto-generated before each commit. Total items in the library: 26_

#### Table of contents:

* [es-lib-buffer-local-set-key](#es-lib-buffer-local-set-key)
* [es-lib-buffer-local-set-key](#es-lib-buffer-local-set-key)
* [es-lib-core-functions](#es-lib-core-functions)
* [es-lib-core-functions](#es-lib-core-functions)
* [es-lib-core-macros](#es-lib-core-macros)
* [es-lib-core-macros](#es-lib-core-macros)
* [es-lib-duplicate](#es-lib-duplicate)
* [es-lib-duplicate](#es-lib-duplicate)
* [es-lib-lexical](#es-lib-lexical)
* [es-lib-lexical](#es-lib-lexical)
* [es-lib-number-at-point](#es-lib-number-at-point)
* [es-lib-number-at-point](#es-lib-number-at-point)
* [es-lib-text-navigate](#es-lib-text-navigate)
* [es-lib-text-navigate](#es-lib-text-navigate)
* [es-lib-total-line](#es-lib-total-line)
* [es-lib-total-line](#es-lib-total-line)



## es-lib-buffer-local-set-key


#### Defvars:

* es-buffer-local-mode

## es-lib-buffer-local-set-key


#### Defvars:

* es-buffer-local-mode

## es-lib-core-functions


#### Defvars:

* es-figlet-font-history
* es-figlet-phrase-history

#### Commands:

* es-new-empty-buffer

## es-lib-core-functions


#### Defvars:

* es-figlet-font-history
* es-figlet-phrase-history

#### Commands:

* es-new-empty-buffer

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


## es-lib-duplicate


## es-lib-lexical


## es-lib-lexical


## es-lib-number-at-point


## es-lib-number-at-point


## es-lib-text-navigate


## es-lib-text-navigate


## es-lib-total-line


## es-lib-total-line
