(in-package :cl-user)
(defpackage :inquisitor-test.external-format
  (:use :cl
        :inquisitor.external-format
        :inquisitor.names
        :prove))
(in-package :inquisitor-test.external-format)

(plan 2)

(subtest "make-external-format"
  (let* ((utf-8 (dependent-name :utf-8))
         (lf (dependent-name :lf)))
    (declare (ignorable lf))
    (is (make-external-format :utf-8 :lf)
        #+clisp (ext:make-encoding :charset utf-8 :line-terminator lf)
        #+ecl (list utf-8 lf)
        #+sbcl utf-8
        #+ccl (ccl:make-external-format :character-encoding utf-8
                                        :line-termination lf)
        #+abcl (list utf-8 :eol-style lf))))

(subtest "make external-format for flexi-streams"
  #+flexi-streams
  ;;; how can I run it?
  (let* ((utf-8 (dependent-name :utf-8 :flexi-name))
         (lf (dependent-name :lf :flexi-name))
         (ef (make-external-format utf-8 lf :type :flexi :little-endian nil)))
    (is (slot-value ef 'flex::name) :utf-8)
    (is (slot-value ef 'flex::eol-style) :lf)
    (is (slot-value ef 'flex::little-endian) nil)))

(finalize)
