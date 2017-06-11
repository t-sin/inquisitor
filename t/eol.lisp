(in-package :cl-user)
(defpackage inquisitor.eol-test
  (:use :cl
        :inquisitor.eol
        :prove)
  (:import-from :inquisitor.util
                :with-byte-array))
(in-package :inquisitor.eol-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 5)


(defun test-eol (path eol)
  (with-open-file (in (asdf:system-relative-pathname :inquisitor path)
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (with-byte-array (vec (file-length in))
      (read-sequence vec in)
      (is (eol-guess-from-vector vec) eol))))


(test-eol "t/data/ascii/ascii-cr.txt" :cr)
(test-eol "t/data/ascii/ascii-crlf.txt" :crlf)
(test-eol "t/data/ascii/ascii-lf.txt" :lf)
(test-eol "t/data/ascii/ascii-lfcr.txt" :lf)

(subtest "If file has no newline then return NIL"
  (test-eol "t/data/ascii/ascii.txt" nil)
  (test-eol "t/data/ascii/empty.txt" nil))


(finalize)
