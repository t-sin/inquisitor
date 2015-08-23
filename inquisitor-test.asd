#|
  This file is a part of inquisitor project.
  Copyright (c) 2015 gray (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage inquisitor-test-asd
  (:use :cl :asdf))
(in-package :inquisitor-test-asd)

(defsystem inquisitor-test
  :author "gray"
  :license ""
  :depends-on (:inquisitor
               :babel
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "inquisitor"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
