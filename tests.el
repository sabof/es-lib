(require 'ert)

(ert-deftest es--change-number-at-point ()
  (with-temp-buffer
    ;; Simple tests
    (insert "10")
    (backward-char)
    (es--change-number-at-point)
    (should (string-equal "11" (buffer-string)))
    (should (= (point) 2))

    (backward-char)
    (es--change-number-at-point)
    (should (string-equal "21" (buffer-string)))
    (should (= (point) 1))

    (goto-char (line-end-position))
    (insert " ")
    (es--change-number-at-point)
    (should (string-equal "22 " (buffer-string)))
    (should (= (point) 4))

    (es--change-number-at-point -10)
    (should (string-equal "12 " (buffer-string)))
    (should (= (point) 4))

    (es--change-number-at-point -30)
    (should (string-equal "-18 " (buffer-string)))
    (should (= (point) 5))

    (goto-char 2)
    (es--change-number-at-point -10)
    (should (string-equal "-118 " (buffer-string)))
    (should (= (point) 3))

    ;; Point on minus -- no change
    (goto-char 1)
    (es--change-number-at-point -10)
    (should (string-equal "-118 " (buffer-string)))
    (should (= (point) 1))

    (progn
      (goto-char 4)
      (es--change-number-at-point 118)
      (should (string-equal "0 " (buffer-string)))
      (should (= (point) 1)))

    ;; Digit reduction with a negative, correct point position
    (progn
      (erase-buffer)
      (insert "-118")
      (goto-char 2)
      (es--change-number-at-point)
      (should (= (point) 2)))

    ;; Syntax tests
    (erase-buffer)
    (scheme-mode)
    (insert "symbol-1")
    (es--change-number-at-point)
    (should (string-equal "symbol-2" (buffer-string)))
    (es--change-number-at-point -2)
    (should (string-equal "symbol-0" (buffer-string)))
    (es--change-number-at-point -2)
    (should (string-equal "symbol-0" (buffer-string)))

    ))
