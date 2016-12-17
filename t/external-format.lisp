(in-package :cl-user)
(defpackage :inquisitor-test.external-format
  (:use :cl
        :inquisitor.external-format
        :inquisitor.names
        :prove))
(in-package :inquisitor-test.external-format)

(plan 1)


(subtest "make-external-format"
  (let* ((utf8 (name-on-impl :utf8))
         (lf (name-on-impl :lf)))
    (declare (ignorable lf))
    (is (make-external-format :utf8 :lf)
        #+clisp (ext:make-encoding :charset utf8 :line-terminator lf)
        #+ecl (list utf8 lf)
        #+sbcl utf8
        #+ccl (ccl:make-external-format :character-encoding utf8
                                        :line-termination lf)
        #+abcl (list utf8 :eol-style lf))))


(finalize)
