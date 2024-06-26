(defsystem "astra-lambda"
  :version "0.8"
  :author "C. Huffenbach"
  :mailto "nphard1234@gmail.com"
  :license "MIT"
  :depends-on (#:cffi
	       #:cl-ppcre
	       #:clgplot)
  :components ((:file "src/package")
	       (:file "src/main"))
:description "a system for astronomical calculations and catalog data evaluation"
  :in-order-to ((test-op (test-op "astra-lambda/tests"))))

(defsystem "astra-lambda/tests"
  :author "C. Huffenbach"
  :license "MIT"
  :depends-on ("astra-lambda"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for astra-lambda"
  :perform (test-op (op c) (symbol-call :rove :run c)))
