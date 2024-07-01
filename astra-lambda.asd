(defsystem "astra-lambda"
  :version "0.8"
  :author "C. Huffenbach"
  :mailto "nphard1234@gmail.com"
  :license "MIT"
  :depends-on (#:cffi
	       #:cl-ppcre)
  :components ((:file "src/package")
	       (:file "src/main"))
:description "a system for astronomical calculations and catalog data evaluation")

