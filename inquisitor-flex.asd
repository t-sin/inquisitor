(in-package :cl-user)
(defpackage inquisitor-flex-asd
  (:use :cl :asdf))
(in-package :inquisitor-flex-asd)

(defsystem inquisitor-flex
  :version "0.1"
  :description "Inquisitor with flexi-streams support"
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on (:flexi-streams
               :inquisitor)
  :components ((:module "src"
                :components
                ((:module "ext"
                  :components
                  ((:file "flexi-streams"))))))
  :in-order-to ((test-op (test-op inquisitor-flex-test))))
