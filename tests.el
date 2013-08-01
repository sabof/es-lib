(require 'ert)

(ert-deftest es--change-number-at-point ()
  (with-temp-buffer
    (insert "10")
    (backward-char)
    (es--change-number-at-point)
    (should (equal "11" (buffer-string)))
    (should (= (point) 2))

    (backward-char)
    (es--change-number-at-point)
    (should (equal "21" (buffer-string)))
    (should (= (point) 1))

    (goto-char (line-end-position))
    (insert " ")
    (es--change-number-at-point)
    (should (equal "22 " (buffer-string)))
    (should (= (point) 4))

    (es--change-number-at-point -10)
    (should (equal "12 " (buffer-string)))
    (should (= (point) 4))

    (es--change-number-at-point -30)
    (should (equal "-18 " (buffer-string)))
    (should (= (point) 5))

    (goto-char 2)
    (es--change-number-at-point -10)
    (should (equal "-118 " (buffer-string)))
    (should (= (point) 3))

    (forward-char)
    (es--change-number-at-point 118)
    (should (equal "0 " (buffer-string)))
    (should (= (point) 1))
    ))
