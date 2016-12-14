#|
  This file is a part of inquisitor project.
  Copyright (c) 2015 Shinichi Tanaka (shinichi.tanaka45@gmail.com)
|#

#|
  Encoding/end-of-line detecter and wrapper of external-format for Common Lisp

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
               :anaphora
               :metabang-bind)
  :components ((:module "src"
                :components
                ((:module "encoding"
                  :components
                  ((:file "dfa")
                   (:file "table" :depends-on ("dfa"))
                   (:file "guess" :depends-on ("dfa" "table"))))
                 (:file "eol")
                 (:file "names")
                 (:file "util")
                 (:file "inquisitor" :depends-on ("util")))))
  :description "Encoding/end-of-line detecter and of external-format wrapper for Common Lisp"
  :in-order-to ((test-op (test-op inquisitor-test))))
