(defsystem :hbook
  :version "0.0.1"
  :description "FIXME"
  :author "FIXME"
  :license "FIXME"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op :hbook/test)))
  :depends-on (:arrows
               ;; FIXME: Though not an explicit run-time dependency,
               ;; for some reason I needed to add this for GitHub
               ;; Action CI build to succeed.  This merits more
               ;; investigation:
               :1am)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main" :depends-on ("package"))))))

(defsystem :hbook/test
  :description "FIXME"
  :author "FIXME"
  :license "FIXME"
  :depends-on (:hbook :1am)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test"))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string "hbook.test:run-tests"))))
