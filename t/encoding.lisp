(in-package :cl-user)
(defpackage inquisitor.encoding-test
  (:use :cl
        :inquisitor.encoding.guess
        :prove)
  (:import-from :inquisitor.util
                :with-byte-array)
  (:import-from :inquisitor-test.util
                :get-test-data))
(in-package :inquisitor.encoding-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 13)


(defun test-enc (path scm enc)
  (with-open-file (in (get-test-data path)
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (with-byte-array (vec (file-length in))
      (read-sequence vec in)
      (multiple-value-bind (encoding treatable)
          (ces-guess-from-vector vec scm)
        (is encoding enc)
        (when treatable
          (diag (format nil " ; ~a cannot treat ~a"
                        (lisp-implementation-type)
                        encoding)))))))


(subtest "list available schemes"
  (is (list-available-scheme)
      '(:jp :tw :cn :kr :ru :ar :tr :gr :hw :pl :bl)))

(subtest "encoding -- not supported scheme"
  (is-error (test-enc "data/empty.txt" :not-supported :utf-8) 'error))

(subtest "encoding -- jp"
  (test-enc "data/ascii/empty.txt" :jp :utf-8)
  (test-enc "data/ascii/ascii.txt" :jp :utf-8)

  (test-enc "data/ja/eucjp.txt" :jp :euc-jp)
  (test-enc "data/ja/jis.txt" :jp :iso-2022-jp)
  (test-enc "data/ja/sjis.txt" :jp :cp932)

  (test-enc "data/unicode/utf-8.txt" :jp :utf-8)
  (test-enc "data/unicode/ucs2be.txt" :jp :ucs-2be)
  (test-enc "data/unicode/ucs2le.txt" :jp :ucs-2le)
  (test-enc "data/unicode/utf-16.txt" :jp :utf-16))

(subtest "encoding -- tw"
  (test-enc "data/ascii/empty.txt" :tw :utf-8)
  (test-enc "data/ascii/ascii.txt" :tw :utf-8)

  (test-enc "data/tw/big5.txt" :tw :big5)
  (diag "iso2022 is equivalent to euc-tw, probably... (https://en.wikipedia.org/wiki/CNS_11643)")
  (test-enc "data/tw/euctw.txt" :tw :iso-2022-tw)

  (test-enc "data/unicode/utf-8.txt" :tw :utf-8))

(subtest "encoding -- cn"
  (test-enc "data/ascii/empty.txt" :cn :utf-8)
  (test-enc "data/ascii/ascii.txt" :cn :utf-8)

  (test-enc "data/cn/gb2312.txt" :cn :gb2312)
  (test-enc "data/cn/gb18030.txt" :cn :gb18030)
  (test-enc "data/cn/iso2022cn.txt" :cn :iso-2022-cn)

  (test-enc "data/unicode/utf-8.txt" :cn :utf-8))

(subtest "encoding -- kr"
  (test-enc "data/ascii/empty.txt" :kr :utf-8)
  (test-enc "data/ascii/ascii.txt" :kr :utf-8)

  (test-enc "data/kr/euckr.txt" :kr :euc-kr)
  (test-enc "data/kr/johab.txt" :kr :johab)
  (test-enc "data/kr/iso2022kr.txt" :kr :iso-2022-kr)

  (test-enc "data/unicode/utf-8.txt" :kr :utf-8))

(subtest "encoding -- ar"
  (test-enc "data/ascii/empty.txt" :ar :utf-8)
  (test-enc "data/ascii/ascii.txt" :ar :utf-8)

  (test-enc "data/ar/iso8859-6.txt" :ar :iso-8859-6)
  (test-enc "data/ar/cp1256.txt" :ar :cp1256)

  (test-enc "data/unicode/utf-8.txt" :ar :utf-8))

(subtest "encoding -- gr"
  (test-enc "data/ascii/empty.txt" :gr :utf-8)
  (test-enc "data/ascii/ascii.txt" :gr :utf-8)

  (test-enc "data/gr/iso8859-7.txt" :gr :iso-8859-7)
  (diag "in range of greek character, cp1253 is subset of iso8859-7, probably")
  (test-enc "data/gr/cp1253.txt" :gr :cp1253)

  (test-enc "data/unicode/utf-8.txt" :gr :utf-8))

(subtest "encoding -- hw"
  (test-enc "data/ascii/empty.txt" :hw :utf-8)
  (test-enc "data/ascii/ascii.txt" :hw :utf-8)

  (test-enc "data/hw/iso8859-8.txt" :hw :iso-8859-8)
  (test-enc "data/hw/cp1255.txt" :hw :cp1255)

  (diag "*TODO!* iso8859-8 does not has vowels (called 'nikud'), thus that case must be added.")

  (test-enc "data/unicode/utf-8.txt" :hw :utf-8))

(subtest "encoding -- tr"
  (test-enc "data/ascii/empty.txt" :tr :utf-8)
  (test-enc "data/ascii/ascii.txt" :tr :utf-8)

  (test-enc "data/tr/iso8859-9.txt" :tr :iso-8859-9)
  (test-enc "data/tr/cp1254.txt" :tr :cp1254)

  (test-enc "data/unicode/utf-8.txt" :tr :utf-8))

(subtest "encoding -- ru"
  (test-enc "data/ascii/empty.txt" :ru :utf-8)
  (test-enc "data/ascii/ascii.txt" :ru :utf-8)

  (test-enc "data/ru/iso8859-5.txt" :ru :iso-8859-5)
  (test-enc "data/ru/koi8r.txt" :ru :koi8-r)
  (test-enc "data/ru/koi8u.txt" :ru :koi8-u)
  (test-enc "data/ru/cp866.txt" :ru :cp866)
  (test-enc "data/ru/cp1251.txt" :ru :cp1251)

  (test-enc "data/unicode/utf-8.txt" :ru :utf-8))

(subtest "encoding -- pl"
  (test-enc "data/ascii/empty.txt" :pl :utf-8)
  (test-enc "data/ascii/ascii.txt" :pl :utf-8)

  (test-enc "data/pl/iso8859-2.txt" :pl :iso-8859-2)
  (test-enc "data/pl/cp1250.txt" :pl :cp1250)

  (test-enc "data/unicode/utf-8.txt" :pl :utf-8))

(subtest "encoding -- bl"
  (test-enc "data/ascii/empty.txt" :bl :utf-8)
  (test-enc "data/ascii/ascii.txt" :bl :utf-8)

  (test-enc "data/bl/iso8859-13.txt" :bl :iso-8859-13)
  (test-enc "data/bl/cp1257.txt" :bl :cp1257)

  (test-enc "data/unicode/utf-8.txt" :bl :utf-8))


(finalize)
