#|
  This file is a part of inquisitor project.
  Copyright (c) 2015 Shinichi Tanaka (shinichi.tanaka45@gmail.com)
|#

#|
  Encoding/end-of-line detection and external-format abstraction for Common Lisp

  Author: Shinichi Tanaka (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage inquisitor-asd
  (:use :cl :asdf))
(in-package :inquisitor-asd)

(defsystem inquisitor
  :version "0.5"
  :author "Shinichi Tanaka"
  :license "MIT"
  :depends-on (:alexandria
               :anaphora)
  :components ((:module "src"
                :components
                ((:module "encoding"
                  :components
                  ((:file "dfa")
                   (:file "table" :depends-on ("dfa"))
                   (:file "guess" :depends-on ("dfa" "table"))))
                 (:file "eol")
                 (:file "names")
                 (:file "external-format" :depends-on ("names"))
                 (:file "util")
                 (:file "inquisitor"
                  :depends-on ("encoding" "eol" "names" "external-format" "util")))))
  :description "Encoding/end-of-line detection and of external-format abstraction for Common Lisp"
  :in-order-to ((test-op (test-op inquisitor-test))))
