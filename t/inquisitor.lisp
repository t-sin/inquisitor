(in-package :cl-user)
(defpackage inquisitor-test
  (:use :cl
        :inquisitor
        :inquisitor.keyword
        :prove))
(in-package :inquisitor-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 12)

(subtest "end-of-line"
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


;;;; encoding
(defun test-enc (path scm enc)
  (with-open-file (in (merge-pathnames path *load-truename*)
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (multiple-value-bind (encoding treatable)
        (detect-encoding in scm)
      (is encoding enc)
      (when treatable
        (diag (format nil " ; ~a cannot treat ~a"
                      (lisp-implementation-type)
                      encoding))))))

(subtest "encoding -- jp"
  (test-enc "dat/empty.txt" :jp (utf8-keyword))
  (test-enc "dat/ascii.txt" :jp (utf8-keyword))

  (test-enc "dat/ja/eucjp-lf.ja" :jp (eucj-keyword))

  (test-enc "dat/ja/jis-lf.ja" :jp (iso-2022-jp-keyword))

  (test-enc "dat/ja/sjis-crlf.ja" :jp (sjis-keyword))

  (test-enc "dat/ja/ucs2-be-lf.ja" :jp (ucs-2be-keyword))
  (test-enc "dat/ja/ucs2-le-lf.ja" :jp (ucs-2le-keyword))
  (test-enc "dat/ja/utf16-lf.ja" :jp (utf16-keyword)) ; not UCS-2 because of surrogate pair

  (test-enc "dat/ja/utf8-cr.ja" :jp (utf8-keyword))
  (test-enc "dat/ja/utf8-crlf.ja" :jp (utf8-keyword))
  (test-enc "dat/ja/utf8-lf.ja" :jp (utf8-keyword)))


(finalize)
