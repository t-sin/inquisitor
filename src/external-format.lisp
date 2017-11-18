(in-package :cl-user)
(defpackage :inquisitor.external-format
  (:use :cl)
  (:export :make-external-format)
  (:import-from :inquisitor.names
                :dependent-name))
(in-package :inquisitor.external-format)

(defgeneric make-external-format
    (type enc eol &rest args &key &allow-other-keys))

(defmethod make-external-format
    ((type (eql :impl)) enc eol &rest args &key &allow-other-keys)
  (let ((enc-on-impl (dependent-name enc))
        (eol-on-impl (dependent-name eol)))
    (declare (ignorable eol-on-impl))
    #+clisp (ext:make-encoding :charset enc-on-impl
                               :line-terminator eol-on-impl)
    #+ecl `(,enc-on-impl ,eol-on-impl)
    #+sbcl enc-on-impl
    #+ccl (ccl:make-external-format :character-encoding enc-on-impl
                                    :line-termination eol-on-impl)
    #+abcl `(,enc-on-impl :eol-style ,eol-on-impl)
    #-(or clisp ecl sbcl ccl abcl)
    (error "your implementation is not supported.")))
