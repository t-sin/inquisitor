(in-package :cl-user)
(defpackage :inquisitor.external-format
  (:use :cl)
  (:export :make-external-format)
  (:import-from :inquisitor.names
                :name-on-impl))
(in-package :inquisitor.external-format)


(defun make-external-format (enc eol)
  (let ((enc-on-impl (name-on-impl enc))
        (eol-on-impl (name-on-impl eol)))
    #+clisp (ext:make-encoding :charset enc-on-impl
                               :line-terminator eol-on-impl)
    #+ecl `(,enc-on-impl ,eol-on-impl)
    #+sbcl enc-on-impl
    #+ccl (ccl:make-external-format :character-encoding enc-on-impl
                                    :line-termination eol-on-impl)
    #+abcl `(,enc-on-impl :eol-style ,eol-on-impl)
    #-(or clisp ecl sbcl ccl abcl)
    (error "your implementation is not supported.")))
