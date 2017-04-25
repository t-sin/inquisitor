(in-package :cl-user)
(defpackage :inquisitor-test.external-format
  (:use :cl
        :inquisitor.external-format
        :inquisitor.names
        :prove))
(in-package :inquisitor-test.external-format)

(plan 1)


(subtest "make-external-format"
  (let* ((utf-8 (independent-name :utf-8))
         (lf (independent-name :lf)))
    (declare (ignorable lf))
    (is (make-external-format :utf-8 :lf)
        #+clisp (ext:make-encoding :charset utf-8 :line-terminator lf)
        #+ecl (list utf-8 lf)
        #+sbcl utf-8
        #+ccl (ccl:make-external-format :character-encoding utf-8
                                        :line-termination lf)
        #+abcl (list utf-8 :eol-style lf))))


(finalize)
