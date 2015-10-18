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
  (is-error (test-enc "dat/empty.txt" :not-supported :utf8) 'error))

(subtest "encoding -- jp"
  (test-enc "dat/empty.txt" :jp :utf8)
  (test-enc "dat/ascii.txt" :jp :utf8)

  (test-enc "dat/ja/eucjp-lf.ja" :jp :euc-jp)

  (test-enc "dat/ja/jis-lf.ja" :jp :iso2022-jp)

  (test-enc "dat/ja/sjis-crlf.ja" :jp :cp932)

  (test-enc "dat/ja/ucs2-be-lf.ja" :jp :ucs-2be)
  (test-enc "dat/ja/ucs2-le-lf.ja" :jp :ucs-2le)
  (diag "not UCS-2 because of surrogate pair")
  (test-enc "dat/ja/utf16-lf.ja" :jp :utf16)

  (test-enc "dat/ja/utf8-cr.ja" :jp :utf8)
  (test-enc "dat/ja/utf8-crlf.ja" :jp :utf8)
  (test-enc "dat/ja/utf8-lf.ja" :jp :utf8))

(subtest "encoding -- tw"
  (test-enc "dat/empty.txt" :tw :utf8)
  (test-enc "dat/ascii.txt" :tw :utf8)

  (test-enc "dat/tw/big5-lf.tw" :tw :big5)
  (diag "iso2022 is equivalent to euc-tw, probably... (https://en.wikipedia.org/wiki/CNS_11643)")
  (test-enc "dat/tw/euctw-lf.tw" :tw :iso2022-tw)
  (test-enc "dat/tw/utf8-lf.tw" :tw :utf8))

(subtest "encoding -- cn"
  (test-enc "dat/empty.txt" :cn :utf8)
  (test-enc "dat/ascii.txt" :cn :utf8)

  (test-enc "dat/cn/gb2312-lf.cn" :cn :gb2312)
  (test-enc "dat/cn/gb18030-lf.cn" :cn :gb18030)
  (test-enc "dat/cn/iso2022-cn-lf.cn" :cn :iso2022-cn)
  (test-enc "dat/cn/utf8-lf.cn" :cn :utf8))

(subtest "encoding -- kr"
  (test-enc "dat/empty.txt" :kr :utf8)
  (test-enc "dat/ascii.txt" :kr :utf8)

  (test-enc "dat/kr/euckr-lf.kr" :kr :euc-kr)
  (test-enc "dat/kr/johab-lf.kr" :kr :johab)
  (test-enc "dat/kr/iso2022kr-lf.kr" :kr :iso2022-kr)
  (test-enc "dat/kr/utf8-lf.kr" :kr :utf8))

(subtest "encoding -- ar"
  (test-enc "dat/empty.txt" :ar :utf8)
  (test-enc "dat/ascii.txt" :ar :utf8)

  (test-enc "dat/ar/iso8859-6-lf.ar" :ar :iso8859-6)
  (test-enc "dat/ar/cp1256-lf.ar" :ar :cp1256)
  (test-enc "dat/ar/utf8-lf.ar" :ar :utf8))

(subtest "encoding -- gr"
  (test-enc "dat/empty.txt" :gr :utf8)
  (test-enc "dat/ascii.txt" :gr :utf8)

  (test-enc "dat/gr/iso8859-7.gr" :gr :iso8859-7)
  (diag "in range of greek character, cp1253 is subset of iso8859-7, probably")
  (test-enc "dat/gr/cp1253.gr" :gr :cp1253)

  (test-enc "dat/gr/utf8-lf.gr" :gr :utf8))

(subtest "encoding -- hw"
  (test-enc "dat/empty.txt" :hw :utf8)
  (test-enc "dat/ascii.txt" :hw :utf8)

  (subtest "with vowels"
    (diag "iso8859-8 does not has vowels (called 'nikud')")
    (test-enc "dat/hw/cp1255-lf_with-vowels.hw" :hw :cp1255)
    (test-enc "dat/hw/utf8-lf_with-vowels.hw" :hw :utf8))

  (subtest "without vowels"
    (test-enc "dat/hw/iso8859-8-lf_without-vowels.hw" :hw :iso8859-8)
    (test-enc "dat/hw/cp1255-lf_without-vowels.hw" :hw :cp1255)
    (test-enc "dat/hw/utf8-lf_without-vowels.hw" :hw :utf8)))

(subtest "encoding -- tr"
  (test-enc "dat/empty.txt" :tr :utf8)
  (test-enc "dat/ascii.txt" :tr :utf8)

  (test-enc "dat/tr/iso8859-9-lf.tr" :tr :iso8859-9)
  (test-enc "dat/tr/cp1254-lf.tr" :tr :cp1254)
  (test-enc "dat/tr/utf8-lf.tr" :tr :utf8))

(subtest "encoding -- ru"
  (test-enc "dat/empty.txt" :ru :utf8)
  (test-enc "dat/ascii.txt" :ru :utf8)

  (test-enc "dat/ru/iso8859-5-lf.ru" :ru :iso8859-5)
  (test-enc "dat/ru/koi8-r-lf.ru" :ru :koi8-r)
  (test-enc "dat/ru/koi8-u-lf.ru" :ru :koi8-u)
  (test-enc "dat/ru/cp866-lf.ru" :ru :cp866)
  (test-enc "dat/ru/cp1251-lf.ru" :ru :cp1251)
  (test-enc "dat/ru/utf8-lf.ru" :ru :utf8))

(subtest "encoding -- pl"
  (test-enc "dat/empty.txt" :pl :utf8)
  (test-enc "dat/ascii.txt" :pl :utf8)

  (test-enc "dat/pl/iso8859-2-lf.pl" :pl :iso8859-2)
  (test-enc "dat/pl/cp1250-lf.pl" :pl :cp1250)
  (test-enc "dat/pl/utf8-lf.pl" :pl :utf8))

(subtest "encoding -- bl"
  (test-enc "dat/empty.txt" :bl :utf8)
  (test-enc "dat/ascii.txt" :bl :utf8)

  (test-enc "dat/bl/iso8859-13-lf.bl" :bl :iso8859-13)
  (test-enc "dat/bl/cp1257-lf.bl" :bl :cp1257)
  (test-enc "dat/bl/utf8-lf.bl" :bl :utf8))


(finalize)
