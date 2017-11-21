(in-package :cl-user)
(defpackage inquisitor-flex-test-asd
  (:use :cl :asdf))
(in-package :inquisitor-flex-test-asd)

(defsystem inquisitor-flex-test
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on (:inquisitor-flex)
  :components ((:module "t"
                :components
                ((:module "ext"
                  :components
                  ((:file "flexi-streams"))))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
