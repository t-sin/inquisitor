(in-package :cl-user)
(defpackage inquisitor.names-test
  (:use :cl
        :inquisitor.names
        :prove))
(in-package :inquisitor.names-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 16)

(is +available-encodings+
      ;; unicode
    '(:utf-8
      :ucs-2le
      :ucs-2be
      :utf-16
      ;; japanese
      :iso-2022-jp
      :euc-jp
      :cp932
      ;; tiwanese
      :big5
      :iso-2022-tw
      ;; chinese
      :gb2312
      :gb18030
      :iso-2022-cn
      ;; korean
      :euc-kr
      :johab
      :iso-2022-kr
      ;; arabic
      :iso-8859-6
      :cp1256
      ;; greek
      :iso-8859-7
      :cp1253
      ;; hebrew
      :iso-8859-8
      :cp1255
      ;; turkish
      :iso-8859-9
      :cp1254
      ;; russian
      :iso-8859-5
      :koi8-r
      :koi8-u
      :cp866
      :cp1251
      ;; polish
      :iso-8859-2
      :cp1250
      ;; baltic
      :iso-8859-13
      :cp1257))

(is +available-eols+ '(:lf :cr :crlf))

(defvar +cannot-treat+ :cannot-treat)

(subtest "unicode"
  (is (independent-name :utf-8)
      #+clisp charset:utf-8
      #+ecl :utf-8
      #+sbcl :utf-8
      #+ccl :utf-8
      #+abcl :utf-8
      #+allegro :utf8
      #+lispworks :utf-8)
  (is (independent-name :ucs-2le)
      #+clisp charset:unicode-16-little-endian  ;; = ucs-2 = unicode-16
      #+ecl :utf-16le  ;; = :ucs-2le
      #+sbcl :utf-16le
      #+ccl :utf-16le
      #+abcl :utf-16le
      #+allegro +cannot-treat+
      #+lispworks '(:unicode :little-endian))
  (is (independent-name :ucs-2be)
      #+clisp charset:unicode-16-big-endian
      #+ecl :utf-16be  ;; = :ucs-2be
      #+sbcl :utf-16be
      #+ccl :utf-16be
      #+abcl :utf-16be
      #+allegro +cannot-treat+
      #+lispworks '(:unicode :big-endian))
  (is (independent-name :utf-16)
      #+clisp charset:utf-16
      #+ecl :utf-16  ;; = :ucs-2
      #+sbcl +cannot-treat+
      #+ccl :utf-16
      #+abcl :utf-16
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+))

(subtest "japanese"
 (is (independent-name :iso-2022-jp)
      #+clisp charset:iso-2022-jp
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :iso-2022-jp
      #+allegro :jis
      #+lispworks :jis)
 (is (independent-name :euc-jp)
      #+clisp charset:euc-jp
      #+ecl +cannot-treat+
      #+sbcl :euc-jp
      #+ccl :euc-jp
      #+abcl :euc-jp
      #+allegro :euc  ; I tried.
      #+lispworks :euc-jp)
 (is (independent-name :cp932)
      #+clisp charset:cp932
      #+ecl :windows-cp932
      #+sbcl :shift_jis
      #+ccl :windows-31j
      #+abcl :|x-MS932_0213|
      #+allegro :shiftjis
      #+lispworks :sjis))

(subtest "tiwanese"
 (is (independent-name :big5)
      #+clisp charset:big5
      #+ecl :windows-cp950
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :|Big5|
      #+allegro :big5
      #+(and lispworks windows) '(win32:code-page :id 950))
 (is (independent-name :iso-2022-tw)
      #+clisp charset:euc-tw
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :|x-EUC-TW|
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+))

(subtest "chinese"
 (is (independent-name :gb2312)  ;; = EUC-CN, GBK, cp936
      #+clisp charset:gbk
      #+ecl :windows-cp936
      #+sbcl :gbk
      #+ccl :cp936
      #+abcl :gbk
      #+allegro +cannot-treat+
      #+lispworks :gbk)
 (is (independent-name :gb18030)
      #+clisp charset:gb18030
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :gb18030
      #+allegro :gb18030
      #+lispworks +cannot-treat+)
 (is (independent-name :iso-2022-cn)
      #+clisp charset:iso-2022-cn
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :iso-2022-cn
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+))

(subtest "korean"
 (is (independent-name :euc-kr)
      #+clisp charset:euc-kr
      #+ecl :windows-cp949
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :euc-kr
      #+allegro :949
      #+(and lispworks windows) '(win32:code-page :id 949)
 (is (independent-name :johab)
      #+clisp charset:johab
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :|x-Johab|
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+))
 (is (independent-name :iso-2022-kr)
      #+clisp charset:iso-2022-kr
      #+ecl +cannot-treat+
      #+sbcl +cannot-treat+
      #+ccl +cannot-treat+
      #+abcl :iso-2022-kr
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+))

(subtest "arabic"
 (is (independent-name :iso-8859-6)
      #+clisp charset:iso-8859-6
      #+ecl :iso-8859-6
      #+sbcl :iso-8859-6
      #+ccl :iso-8859-6
      #+abcl :iso-8859-6
      #+allegro iso8859-6
      #+lispworks +cannot-treat+)
 (is (independent-name :cp1256)
      #+clisp charset:windows-1256
      #+ecl :windows-cp1256
      #+sbcl :cp1256
      #+ccl +cannot-treat+
      #+abcl :|windows-1256|
      #+allegro :1256
      #+(and lispworks windows) '(win32:code-page :id 1256)))

(subtest "greek"
 (is (independent-name :iso-8859-7)
      #+clisp charset:iso-8859-7
      #+ecl :iso-8859-7
      #+sbcl :iso-8859-7
      #+ccl :iso-8859-7
      #+abcl :iso-8859-7
      #+allegro :iso8859-7
      #+lispworks +cannot-treat+)
 (is (independent-name :cp1253)
      #+clisp charset:windows-1253
      #+ecl :windows-cp1253
      #+sbcl :cp1253
      #+ccl +cannot-treat+
      #+abcl :|windows-1253|
      #+allegro :1253
      #+(and lispworks windows) '(win32:code-page :id 1253)))

(subtest "hebrew"
  (is (independent-name :iso-8859-8)
      #+clisp charset:iso-8859-8
      #+ecl :iso-8859-8
      #+sbcl :iso-8859-8
      #+ccl :iso-8859-8
      #+abcl :iso-8859-8
      #+allegro :iso8859-8
      #+lispworks +cannot-treat+)
  (is (independent-name :cp1255)
      #+clisp charset:windows-1255
      #+ecl :windows-cp1255
      #+sbcl :cp1255
      #+ccl +cannot-treat+
      #+abcl :|windows-1255|
      #+allegro :1255
      #+(and lispworks windows) '(win32:code-page :id 1255)))

(subtest "turkish"
  (is (independent-name :iso-8859-9)
      #+clisp charset:iso-8859-9
      #+ecl :iso-8859-9
      #+sbcl :iso-8859-9
      #+ccl :iso-8859-9
      #+abcl :iso-8859-9
      #+allegro :iso8859-9
      #+lispworks +cannot-treat+)
  (is (independent-name :cp1254)
      #+clisp charset:windows-1254
      #+ecl :windows-cp1254
      #+sbcl :cp1254
      #+ccl +cannot-treat+
      #+abcl :|windows-1254|
      #+allegro :1254
      #+(and lispworks windows) '(win32:code-page :id 1254)))

(subtest "russian"
  (is (independent-name :iso-8859-5)
      #+clisp charset:iso-8859-5
      #+ecl :iso-8859-5
      #+sbcl :iso-8859-5
      #+ccl :iso-8859-5
      #+abcl :iso-8859-5
      #+allegro :iso8859-5
      #+lispworks +cannot-treat+)
  (is (independent-name :koi8-r)
      #+clisp charset:koi8-r
      #+ecl +cannot-treat+
      #+sbcl :koi8-r
      #+ccl +cannot-treat+
      #+abcl :koi8-r
      #+allegro :koi8-r
      #+lispworks +cannot-treat+)
  (is (independent-name :koi8-u)
      #+clisp charset:koi8-u
      #+ecl +cannot-treat+
      #+sbcl :koi8-u
      #+ccl +cannot-treat+
      #+abcl :koi8-u
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+)
  (is (independent-name :cp866)
      #+clisp charset:cp866
      #+ecl :dos-cp866
      #+sbcl :cp866
      #+ccl +cannot-treat+
      #+abcl :ibm866
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+)
  (is (independent-name :cp1251)
      #+clisp charset:windows-1251
      #+ecl :windows-cp1251
      #+sbcl :cp1251
      #+ccl +cannot-treat+
      #+abcl :|windows-1251|
      #+allegro :1251
      #+(and lispworks windows) '(win32:code-page :id 1251)))

(subtest "polish"
  (is (independent-name :iso-8859-2)
      #+clisp charset:iso-8859-2
      #+ecl :iso-8859-2
      #+sbcl :iso-8859-2
      #+ccl :iso-8859-2
      #+abcl :iso-8859-2
      #+allegro :iso8859-2
      #+lispworks +cannot-treat+)
  (is (independent-name :cp1250)
      #+clisp charset:windows-1250
      #+ecl :windows-cp1250
      #+sbcl :cp1250
      #+ccl +cannot-treat+
      #+abcl :|windows-1250|
      #+allegro :1250
      #+(and lispworks windows) '(win32:code-page :id 1250)))

(subtest "baltic"
  (is (independent-name :iso-8859-13)
      #+clisp charset:iso-8859-13
      #+ecl :iso-8859-13
      #+sbcl :iso-8859-13
      #+ccl :iso-8859-13
      #+abcl :iso-8859-13
      #+allegro +cannot-treat+
      #+lispworks +cannot-treat+)
  (is (independent-name :cp1257)
      #+clisp charset:windows-1257
      #+ecl :windows-cp1257
      #+sbcl :cp1257
      #+ccl +cannot-treat+
      #+abcl :|windows-1257|
      #+allegro :1257
      #+(and lispworks windows) '(win32:code-page :id 1257)))

(subtest "end of line"
  (is (independent-name :lf)
      #+clisp :unix
      #+ecl :lf
      #+sbcl +cannot-treat+
      #+ccl :unix
      #+abcl :lf
      #+allegro :unix  ; https://franz.com/support/documentation/10.1/doc/operators/excl/eol-convention.htm
      #+lispworks :lf)
  (is (independent-name :cr)
      #+clisp :mac
      #+ecl :cr
      #+sbcl +cannot-treat+
      #+ccl :macos
      #+abcl :cr
      #+allegro :mac
      #+lispworks :cr)
  (is (independent-name :crlf)
      #+clisp :dos
      #+ecl :crlf
      #+sbcl +cannot-treat+
      #+ccl :dos
      #+abcl :crlf
      #+allegro :doc
      #+lispworks :crlf))

(subtest "if specified encodings is unicode?"
  (subtest "only unicode returns t"
    (ok (unicode-p :utf-8))
    (ok (unicode-p :ucs-2le))
    (ok (unicode-p :ucs-2be))
    (ok (unicode-p :utf-16)))
  (subtest "other encodings return nil"
    (is (unicode-p :cp932) nil)
    (is (unicode-p :cp932) nil)))

(finalize)
