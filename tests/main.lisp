(defpackage astra-lambda/tests/main
  (:use :cl
        :astra-lambda
        :rove))
(in-package :astra-lambda/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :astra-lambda)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
