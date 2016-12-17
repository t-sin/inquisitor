#|
  This file is a part of inquisitor project.
  Copyright (c) 2015 Shinichi Tanaka (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage inquisitor-test-asd
  (:use :cl :asdf))
(in-package :inquisitor-test-asd)

(defsystem inquisitor-test
  :author "Shinichi Tanaka"
  :license "MIT"
  :depends-on (:inquisitor
               :babel
               :drakma
               :prove)
  :components ((:module "t"
                :components
                ((:file "names")
                 (:file "test-util")
                 (:test-file "util" :depends-on ("test-util"))
                 (:test-file "eol" :depends-on ("test-util"))
                 (:test-file "encoding" :depends-on ("test-util"))
                 (:test-file "external-format")
                 (:test-file "inquisitor" :depends-on ("test-util")))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
