(in-package :cl-user)
(defpackage inquisitor.eol-test
  (:use :cl
        :inquisitor.eol
        :prove)
  (:import-from :inquisitor.util
                :with-byte-array)
  (:import-from :inquisitor-test.util
                :get-test-data))
(in-package :inquisitor.eol-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 5)


(defun test-eol (path eol)
  (with-open-file (in (get-test-data path)
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (with-byte-array (vec (file-length in))
      (read-sequence vec in)
      (is (eol-guess-from-vector vec) eol))))


(test-eol "data/ascii/ascii-cr.txt" :cr)
(test-eol "data/ascii/ascii-crlf.txt" :crlf)
(test-eol "data/ascii/ascii-lf.txt" :lf)
(test-eol "data/ascii/ascii-lfcr.txt" :lf)

(subtest "If file has no newline then return NIL"
  (test-eol "data/ascii/ascii.txt" nil)
  (test-eol "data/ascii/empty.txt" nil))


(finalize)
