(in-package :cl-user)
(defpackage inquisitor-flexi-test-asd
  (:use :cl :asdf))
(in-package :inquisitor-flexi-test-asd)

(defsystem inquisitor-flexi-test
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on (:inquisitor-flexi
               :prove)
  :components ((:module "t"
                :components
                ((:module "ext"
                  :components
                  ((:file "flexi-streams"))))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
