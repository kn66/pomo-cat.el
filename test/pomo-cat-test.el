;;; pomo-cat-test.el --- Tests for pomo-cat -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'pomo-cat)

(ert-deftest pomo-cat-validate-positive-integer-rounds-up-to-one ()
  (should (= 1 (pomo-cat--validate-positive-integer 0.1 "test-duration"))))

(ert-deftest pomo-cat-delay-break-invalid-default-falls-back ()
  (let ((pomo-cat-delay-break-seconds "bad"))
    (should (= 60 (pomo-cat--get-delay-break-seconds)))))

(ert-deftest pomo-cat-delay-break-negative-argument-becomes-zero ()
  (should (= 0 (pomo-cat--get-delay-break-seconds -5))))

(ert-deftest pomo-cat-delay-break-invalid-default-does-not-error ()
  (let ((pomo-cat-delay-break-seconds "bad"))
    (pomo-cat--reset-state)
    (pomo-cat--state-set :in-break t)
    (unwind-protect
        (progn
          (pomo-cat-delay-break)
          (should (eq 'break-delayed (pomo-cat--phase)))
          (should (= 60 (pomo-cat--remaining-break-seconds))))
      (pomo-cat--reset-state))))

(provide 'pomo-cat-test)

;;; pomo-cat-test.el ends here
