(defsystem :hbook
  :version "0.0.1"
  :description "Text-based histograms in Common Lisp inspired by the venerable HBOOK histogramming library from CERN"
  :author "John Jacobsen"
  :license "MIT"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op :hbook/test)))
  :depends-on (:cl-oju :1am)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main" :depends-on ("package"))))))

(defsystem :hbook/test
  :description "Testing for hbook"
  :author "John Jacobsen"
  :license "MIT"
  :depends-on (:hbook :1am :cl-oju)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test"))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string "hbook.test:run-tests"))))
