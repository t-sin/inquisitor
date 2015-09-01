(in-package :cl-user)
(defpackage inquisitor-test
  (:use :cl
        :inquisitor
        :inquisitor.keyword
        :prove)
  (:import-from :inquisitor-test.util
                :get-test-data)
  (:import-from :babel
                :string-to-octets))
(in-package :inquisitor-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 4)


(subtest "make-external-format"
  (let* ((utf8 (utf8-keyword))
         (lf (lf-keyword)))
    (is (make-external-format utf8 lf)
        #+clisp (ext:make-encoding :charset utf8 :line-terminator lf)
        #+ecl (list utf8 lf)
        #+sbcl utf8
        #+ccl (ccl:make-external-format :character-encoding utf8
                                        :line-termination lf)
        #+abcl (list utf8 :eol-style lf))))

(subtest "detect external-format --- from vector"
  (diag "when not byte-array")
  (is-error (detect-external-format "string" :jp) 'error)
  (diag "when cannot treat the encodings (how do I cause it...?)")
  (is-error (detect-external-format "" :jp) 'error)
  (let ((str (string-to-octets "string")))
    (is (detect-external-format str :jp)
        (make-external-format (utf8-keyword) (lf-keyword)))))

(subtest "detect external-format --- from stream"
  (with-output-to-string (out)
    (is-error (detect-external-format out :jp) 'error))
  (with-input-from-string (in "string")
    (is-error (detect-external-format in :jp) 'error))
  (with-open-file (in (get-test-data "dat/ascii.txt")
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (is (detect-external-format in :jp)
        (make-external-format (utf8-keyword) (lf-keyword)))))

(subtest "detect external-format --- from pathname"
  (is-error (detect-external-format "dat/ascii.txt" :jp) 'error)
  (is (detect-external-format (get-test-data "dat/ascii.txt") :jp)
      (make-external-format (utf8-keyword) (lf-keyword))))


(finalize)
