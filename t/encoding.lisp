(in-package :cl-user)
(defpackage inquisitor.encoding-test
  (:use :cl
        :inquisitor.encoding.guess
        :prove)
  (:import-from :inquisitor.util
                :with-byte-array))
(in-package :inquisitor.encoding-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 15)


(defvar +bom-first-byte+ #xfe)
(defvar +bom-second-byte+ #xff)

(defun str2vec (s)
  (apply #'vector (mapcar (lambda (c) (char-code c)) (coerce s 'list))))

(defun test-check-bom (actual-vec expected)
  (is (block inquisitor.encoding.guess::guess-body
        (inquisitor.encoding.guess::check-byte-order-mark
         actual-vec :big-endian :little-endian nil))
      expected))

(subtest "Byte order mark treatment"
  (diag "For details, see Unicode Standard, 2.13. Special Characters, Byte Order Mark (BOM)")
  (subtest "Specified vector is too short"
    (test-check-bom (str2vec "") nil)
    (test-check-bom (str2vec "a") nil))

  (subtest "return nil for normal string"
    (test-check-bom (str2vec "LE") nil)
    (test-check-bom (str2vec "Hy!") nil)
    (test-check-bom (str2vec "Inquisitor") nil))

  (subtest "partially matched for BOM"
    (test-check-bom (vector +bom-first-byte+) nil)
    (test-check-bom (vector +bom-second-byte+) nil)

    (test-check-bom (vector +bom-first-byte+ #x0) nil)
    (test-check-bom (vector +bom-second-byte+ #x0) nil)

    (test-check-bom (vector #x0 +bom-first-byte+) nil)
    (test-check-bom (vector #x0 +bom-second-byte+) nil))

  (subtest "BOM exactly"
    (test-check-bom (vector +bom-first-byte+ +bom-second-byte+) :big-endian)
    (test-check-bom (vector +bom-second-byte+ +bom-first-byte+) :little-endian))

  (subtest "string following BOM"
    (test-check-bom (vector +bom-first-byte+ +bom-second-byte+
                            #.(char-code #\f)
                            #.(char-code #\o)
                            #.(char-code #\o))
                    :big-endian)
    (test-check-bom (vector +bom-second-byte+ +bom-first-byte+
                            #.(char-code #\f)
                            #.(char-code #\o)
                            #.(char-code #\o))
                    :little-endian)))


(defun test-enc (path scm enc)
  (with-open-file (in (asdf:system-relative-pathname :inquisitor path)
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (with-byte-array (vec (file-length in))
      (read-sequence vec in)
      (is (ces-guess-from-vector vec scm) enc))))


(subtest "list available schemes"
  (is (list-available-scheme)
      '(:jp :tw :cn :kr :ru :ar :tr :gr :hw :pl :bl)))

(subtest "encoding -- not supported scheme"
  (is-error (test-enc "t/data/empty.txt" :not-supported :utf-8) 'error))

(subtest "encoding -- jp"
  (test-enc "t/data/ascii/empty.txt" :jp :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :jp :utf-8)

  (test-enc "t/data/ja/eucjp.txt" :jp :euc-jp)
  (test-enc "t/data/ja/jis.txt" :jp :iso-2022-jp)
  (test-enc "t/data/ja/sjis.txt" :jp :cp932)

  (test-enc "t/data/unicode/utf-8.txt" :jp :utf-8)
  (test-enc "t/data/unicode/ucs2be.txt" :jp :ucs-2be)
  (test-enc "t/data/unicode/ucs2le.txt" :jp :ucs-2le)
  (test-enc "t/data/unicode/utf-16.txt" :jp :utf-16))

(subtest "encoding -- tw"
  (test-enc "t/data/ascii/empty.txt" :tw :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :tw :utf-8)

  (test-enc "t/data/tw/big5.txt" :tw :big5)
  (diag "iso2022 is equivalent to euc-tw, probably... (https://en.wikipedia.org/wiki/CNS_11643)")
  (test-enc "t/data/tw/euctw.txt" :tw :iso-2022-tw)

  (test-enc "t/data/unicode/utf-8.txt" :tw :utf-8))

(subtest "encoding -- cn"
  (test-enc "t/data/ascii/empty.txt" :cn :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :cn :utf-8)

  (test-enc "t/data/cn/gb2312.txt" :cn :gb2312)
  (test-enc "t/data/cn/gb18030.txt" :cn :gb18030)
  (test-enc "t/data/cn/iso2022cn.txt" :cn :iso-2022-cn)

  (test-enc "t/data/unicode/utf-8.txt" :cn :utf-8))

(subtest "encoding -- kr"
  (test-enc "t/data/ascii/empty.txt" :kr :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :kr :utf-8)

  (test-enc "t/data/kr/euckr.txt" :kr :euc-kr)
  (test-enc "t/data/kr/johab.txt" :kr :johab)
  (test-enc "t/data/kr/iso2022kr.txt" :kr :iso-2022-kr)

  (test-enc "t/data/unicode/utf-8.txt" :kr :utf-8))

(subtest "encoding -- ar"
  (test-enc "t/data/ascii/empty.txt" :ar :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :ar :utf-8)

  (test-enc "t/data/ar/iso8859-6.txt" :ar :iso-8859-6)
  (test-enc "t/data/ar/cp1256.txt" :ar :cp1256)

  (test-enc "t/data/unicode/utf-8.txt" :ar :utf-8))

(subtest "encoding -- gr"
  (test-enc "t/data/ascii/empty.txt" :gr :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :gr :utf-8)

  (test-enc "t/data/gr/iso8859-7.txt" :gr :iso-8859-7)
  (diag "in range of greek character, cp1253 is subset of iso8859-7, probably")
  (test-enc "t/data/gr/cp1253.txt" :gr :cp1253)

  (test-enc "t/data/unicode/utf-8.txt" :gr :utf-8))

(subtest "encoding -- hw"
  (test-enc "t/data/ascii/empty.txt" :hw :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :hw :utf-8)

  (test-enc "t/data/hw/iso8859-8.txt" :hw :iso-8859-8)
  (test-enc "t/data/hw/cp1255.txt" :hw :cp1255)

  (diag "*TODO!* iso8859-8 does not has vowels (called 'nikud'), thus that case must be added.")

  (test-enc "t/data/unicode/utf-8.txt" :hw :utf-8))

(subtest "encoding -- tr"
  (test-enc "t/data/ascii/empty.txt" :tr :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :tr :utf-8)

  (test-enc "t/data/tr/iso8859-9.txt" :tr :iso-8859-9)
  (test-enc "t/data/tr/cp1254.txt" :tr :cp1254)

  (test-enc "t/data/unicode/utf-8.txt" :tr :utf-8))

(subtest "encoding -- ru"
  (test-enc "t/data/ascii/empty.txt" :ru :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :ru :utf-8)

  (test-enc "t/data/ru/iso8859-5.txt" :ru :iso-8859-5)
  (test-enc "t/data/ru/koi8r.txt" :ru :koi8-r)
  (test-enc "t/data/ru/koi8u.txt" :ru :koi8-u)
  (test-enc "t/data/ru/cp866.txt" :ru :cp866)
  (test-enc "t/data/ru/cp1251.txt" :ru :cp1251)

  (test-enc "t/data/unicode/utf-8.txt" :ru :utf-8))

(subtest "encoding -- pl"
  (test-enc "t/data/ascii/empty.txt" :pl :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :pl :utf-8)

  (test-enc "t/data/pl/iso8859-2.txt" :pl :iso-8859-2)
  (test-enc "t/data/pl/cp1250.txt" :pl :cp1250)

  (test-enc "t/data/unicode/utf-8.txt" :pl :utf-8))

(subtest "encoding -- bl"
  (test-enc "t/data/ascii/empty.txt" :bl :utf-8)
  (test-enc "t/data/ascii/ascii.txt" :bl :utf-8)

  (test-enc "t/data/bl/iso8859-13.txt" :bl :iso-8859-13)
  (test-enc "t/data/bl/cp1257.txt" :bl :cp1257)

  (test-enc "t/data/unicode/utf-8.txt" :bl :utf-8))

(subtest "order (detection state) paasing"
  (diag "segmented detection is equivalent to entirely buffered detection")
  (let ((path (asdf:system-relative-pathname :inquisitor "t/data/unicode/utf-8.txt")))
    (let (enc-segmented order-segmented
          enc-entirely order-entirely)
      (with-open-file (in path
                          :direction :input
                          :element-type '(unsigned-byte 8))
        (with-byte-array (buffer 1000)
          (let (encoding order)
            (loop :named segmented-detection
               :for num-read := (read-sequence buffer in)
               :if (< num-read 1000)
               :do (return-from segmented-detection
                     (ces-guess-from-vector (subseq buffer 0 num-read) :jp order))
               :else
               :do (multiple-value-bind (enc ord)
                       (ces-guess-from-vector buffer :jp order)
                     (setf encoding enc
                           order ord)))
            (setf enc-segmented encoding
                  order-segmented order))))
      (with-open-file (in path
                          :direction :input
                          :element-type '(unsigned-byte 8))
        (with-byte-array (buffer (file-length in))
          (read-sequence buffer in)
          (multiple-value-bind (enc ord)
              (ces-guess-from-vector buffer :jp)
            (setf enc-entirely enc
                  order-entirely ord))))
      (is enc-segmented enc-entirely)
      (is order-segmented order-entirely :test #'equalp))))

(finalize)
