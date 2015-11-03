(in-package :cl-user)
(defpackage inquisitor.names-test
  (:use :cl
        :inquisitor.names
        :prove))
(in-package :inquisitor.names-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 14)

(is (available-encodings)
      ;; unicode
    '(:utf8
      :ucs-2le
      :ucs-2be
      :utf16
      ;; japanese
      :iso2022-jp
      :euc-jp
      :cp932
      ;; tiwanese
      :big5
      :iso2022-tw
      ;; chinese
      :gb2312
      :gb18030
      :iso2022-cn
      ;; korean
      :euc-kr
      :johab
      :iso2022-kr
      ;; arabic
      :iso8859-6
      :cp1256
      ;; greek
      :iso8859-7
      :cp1253
      ;; hebrew
      :iso8859-8
      :cp1255
      ;; turkish
      :iso8859-9
      :cp1254
      ;; russian
      :iso8859-5
      :koi8-r
      :koi8-u
      :cp866
      :cp1251
      ;; polish
      :iso8859-2
      :cp1250
      ;; baltic
      :iso8859-13
      :cp1257))

(defvar +cannot-treat+ :cannot-treat)

(subtest "unicode"
  (is (name-on-impl :utf8)
      #+clisp 'charset:utf-8
      ;; #+ecl :utf-8
      ;; #+sbcl :utf-8
      ;; #+ccl :utf-8
      ;; #+abcl :utf-8
      #-clisp :utf-8)
  (is (name-on-impl :ucs-2le)
      #+clisp 'charset:unicode-16-big-endian  ;; = ucs-2 = unicode-16
      #+ecl :utf-16le  ;; = :ucs-2le
      #+sbcl :utf-16le
      #+ccl :utf-16le
      #+abcl :utf-16le)
  (is (name-on-impl :ucs-2be)
      #+clisp 'charset:unicode-16-big-endian
      #+ecl :utf-16be  ;; = :ucs-2be
      #+sbcl :utf-16be
      #+ccl :utf-16be
      #+abcl :utf-16be)
  (is (name-on-impl :utf16)
      #+clisp 'charset:utf-16
      #+ecl :utf-16  ;; = :ucs-2
      #+sbcl +cannot-treat+
      #+ccl :utf-16
      #+abcl :utf-16))

(subtest "japanese"
 (is (name-on-impl :iso2022-jp)
      #+clisp 'charset:iso-2022-jp
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :iso-2022-jp)
 (is (name-on-impl :euc-jp)
      #+clisp 'charset:euc-jp
      #+ecl +cannot-treat+
      #+sbcl :euc-jp
      #+ccl :euc-jp
      #+abcl :euc-jp)
 (is (name-on-impl :cp932)
      #+clisp 'charset:cp932
      #+ecl :windows-cp932
      #+sbcl :shift_jis
      #+ccl :windows-31j
      #+abcl :|x-MS932_0213|))

(subtest "tiwanese"
 (is (name-on-impl :big5)
      #+clisp 'charset:big5
      #+ecl :windows-cp950
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :|Big5|)
 (is (name-on-impl :iso2022-tw)
      #+clisp 'charset:euc-tw
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :|x-EUC-TW|))

(subtest "chinese"
 (is (name-on-impl :gb2312)  ;; = EUC-CN, GBK, cp936
      #+clisp 'charset:gbk
      #+ecl :windows-cp936
      #+sbcl :gbk
      #+ccl :cp936
      #+abcl :gbk)
 (is (name-on-impl :gb18030)
      #+clisp 'charset:gb18030
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :gb18030)
 (is (name-on-impl :iso2022-cn)
      #+clisp 'charset:iso-2022-cn
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :iso-2022-cn))

(subtest "korean")

(subtest "arabic")

(subtest "greek")

(subtest "hebrew")

(subtest "turkish")

(subtest "russian")

(subtest "polish")

(subtest "baltic")

(subtest "end of line")


(finalize)
