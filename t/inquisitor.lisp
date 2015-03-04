(in-package :cl-user)
(defpackage inquisitor-test
  (:use :cl
        :inquisitor
        :inquisitor.keyword
        :prove))
(in-package :inquisitor-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan nil)

(deftest end-of-line
  (flet ((test-eol (path eol)
           (with-open-file (in (merge-pathnames path *load-truename*)
                            :direction :input
                            :element-type '(unsigned-byte 8))
             (is (detect-end-of-line in) eol))))

    (test-eol "dat/ja/utf8-cr.ja" (cr-keyword))
    (test-eol "dat/ja/utf8-crlf.ja" (crlf-keyword))
    (test-eol "dat/ja/utf8-lf.ja" (lf-keyword))

    ;; if file has no newline then return LF
    (test-eol "dat/ascii.txt" (lf-keyword))
    (test-eol "dat/empty.txt" (lf-keyword))))


(run-test-all)

(finalize)
