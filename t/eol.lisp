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

(plan 4)


(defun test-eol (path eol)
  (with-open-file (in (get-test-data path)
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (with-byte-array (vec (file-length in))
      (read-sequence vec in)
      (is (eol-guess-from-vector vec) eol))))


(test-eol "dat/ja/utf8-cr.ja" :cr)
(test-eol "dat/ja/utf8-crlf.ja" :crlf)
(test-eol "dat/ja/utf8-lf.ja" :lf)

(subtest "If file has no newline then return NIL"
  (test-eol "dat/ascii.txt" nil)
  (test-eol "dat/empty.txt" nil))


(finalize)
