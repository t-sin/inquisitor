(in-package :cl-user)
(defpackage inquisitor.names-test
  (:use :cl
        :inquisitor.names
        :prove))
(in-package :inquisitor.names-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 18)

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

(subtest "unicode"
  (let ((impl-enc #+sbcl :utf-8
             #+ccl :utf-8
             #+clisp charset:utf-8
             #+ecl :utf-8
             #+abcl :utf-8
             #+allegro :utf8
             #+lispworks :utf-8))
    (is (dependent-name :utf-8) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :utf-8)))
  (let ((impl-enc #+sbcl :utf-16le
             #+ccl :utf-16le
             #+clisp charset:unicode-16-little-endian
             #+ecl :utf-16le
             #+abcl :utf-16le
             #+allegro :cannot-treat
             #+lispworks '(:unicode :little-endian)))
    (is (dependent-name :ucs-2le) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :ucs-2le)))
  (let ((impl-enc #+sbcl :utf-16be
             #+ccl :utf-16be
             #+clisp charset:unicode-16-big-endian
             #+ecl :utf-16be
             #+abcl :utf-16be
             #+allegro :cannot-treat
             #+lispworks '(:unicode :big-endian)))
    (is (dependent-name :ucs-2be) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :ucs-2be)))
  (let ((impl-enc #+sbcl :cannot-treat
             #+ccl :utf-16
             #+clisp charset:utf-16
             #+ecl :utf-16
             #+abcl :utf-16
             #+allegro :cannot-treat
             #+lispworks :cannot-treat))
    (is (dependent-name :utf-16) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :utf-16))))

(subtest "japanese"
  (let ((impl-enc #+sbcl :cannot-treat
             #+ccl :cannot-treat
             #+clisp charset:iso-2022-jp
             #+ecl :cannot-treat
             #+abcl :iso-2022-jp
             #+allegro :jis
             #+lispworks :jis))
    (is (dependent-name :iso-2022-jp) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-2022-jp)))
  (let ((impl-enc #+sbcl :euc-jp
             #+ccl :euc-jp
             #+clisp charset:euc-jp
             #+ecl :cannot-treat
             #+abcl :euc-jp
             #+allegro :euc
             #+lispworks :euc-jp))
    (is (dependent-name :euc-jp) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :euc-jp)))
  (let ((impl-enc #+sbcl :shift_jis
             #+ccl :windows-31j
             #+clisp charset:cp932
             #+ecl :windows-cp932
             #+abcl :|x-MS932_0213|
             #+allegro :shiftjis
             #+lispworks :sjis))
    (is (dependent-name :cp932) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp932))))

(subtest "tiwanese"
  (let ((impl-enc #+sbcl :cannot-treat
                  #+ccl :cannot-treat
                  #+clisp charset:big5
                  #+ecl :windows-cp950
                  #+abcl :|Big5|
                  #+allegro :big5
                  #+(and lispworks windows) '(win32:code-page :id 950)))
    (is (dependent-name :big5) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :big5)))
   (let ((impl-enc #+clisp charset:euc-tw
                   #+ecl :cannot-treat
                   #+sbcl :cannot-treat
                   #+ccl :cannot-treat
                   #+abcl :|x-EUC-TW|
                   #+allegro :cannot-treat
                   #+lispworks :cannot-treat))
     (is (dependent-name :iso-2022-tw) impl-enc)
     (unless (eq impl-enc :cannot-treat)
       (is (independent-name impl-enc) :iso-2022-tw))))

(subtest "chinese"
  ;; = EUC-CN, GBK, cp936
  (let ((impl-enc #+sbcl :gbk
                  #+ccl :cp936
                  #+clisp charset:gbk
                  #+ecl :windows-cp936
                  #+abcl :gbk
                  #+allegro :cannot-treat
                  #+lispworks :gbk))
    (is (dependent-name :gb2312) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :gb2312)))
  (let ((impl-enc #+sbcl :cannot-treat
                  #+ccl :cannot-treat
                  #+clisp charset:gb18030
                  #+ecl :cannot-treat
                  #+abcl :gb18030
                  #+allegro :gb18030
                  #+lispworks :cannot-treat))
    (is (dependent-name :gb18030) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :gb18030)))
  (let ((impl-enc #+sbcl :cannot-treat
                  #+ccl :cannot-treat
                  #+clisp charset:iso-2022-cn
                  #+ecl :cannot-treat
                  #+abcl :iso-2022-cn
                  #+allegro :cannot-treat
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-2022-cn) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-2022-cn))))

(subtest "korean"
  (let ((impl-enc #+sbcl :cannot-treat
                  #+ccl :cannot-treat
                  #+clisp charset:euc-kr
                  #+ecl :windows-cp949
                  #+abcl :euc-kr
                  #+allegro :949
                  #+(and lispworks windows) '(win32:code-page :id 949)))
    (is (dependent-name :euc-kr) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :euc-kr)))
  (let ((impl-enc #+sbcl :cannot-treat
                  #+ccl :cannot-treat
                  #+clisp charset:johab
                  #+ecl :cannot-treat
                  #+abcl :|x-Johab|
                  #+allegro :cannot-treat
                  #+lispworks :cannot-treat))
    (is (dependent-name :johab) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :johab)))
  (let ((impl-enc #+sbcl :cannot-treat
                  #+ccl :cannot-treat
                  #+clisp charset:iso-2022-kr
                  #+ecl :cannot-treat
                  #+abcl :iso-2022-kr
                  #+allegro :cannot-treat
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-2022-kr) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc):iso-2022-kr))))

(subtest "arabic"
  (let ((impl-enc #+sbcl :iso-8859-6
                  #+ccl :iso-8859-6
                  #+clisp charset:iso-8859-6
                  #+ecl :iso-8859-6
                  #+abcl :iso-8859-6
                  #+allegro iso8859-6
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-6) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-8859-6)))
  (let ((impl-enc #+sbcl :cp1256
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1256
                  #+ecl :windows-cp1256
                  #+abcl :|windows-1256|
                  #+allegro :1256
                  #+(and lispworks windows) '(win32:code-page :id 1256)))
    (is (dependent-name :cp1256) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1256))))

(subtest "greek"
  (let ((impl-enc #+sbcl :iso-8859-7
                  #+ccl :iso-8859-7
                  #+clisp charset:iso-8859-7
                  #+ecl :iso-8859-7
                  #+abcl :iso-8859-7
                  #+allegro :iso8859-7
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-7) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-8859-7)))
  (let ((impl-enc #+sbcl :cp1253
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1253
                  #+ecl :windows-cp1253
                  #+abcl :|windows-1253|
                  #+allegro :1253
                  #+(and lispworks windows) '(win32:code-page :id 1253)))
    (is (dependent-name :cp1253) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1253))))

(subtest "hebrew"
  (let ((impl-enc #+sbcl :iso-8859-8
                  #+ccl :iso-8859-8
                  #+clisp charset:iso-8859-8
                  #+ecl :iso-8859-8
                  #+abcl :iso-8859-8
                  #+allegro :iso8859-8
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-8) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-8859-8)))
  (let ((impl-enc #+sbcl :cp1255
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1255
                  #+ecl :windows-cp1255
                  #+abcl :|windows-1255|
                  #+allegro :1255
                  #+(and lispworks windows) '(win32:code-page :id 1255)))
    (is (dependent-name :cp1255) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1255))))

(subtest "turkish"
  (let ((impl-enc #+sbcl :iso-8859-9
                  #+ccl :iso-8859-9
                  #+clisp charset:iso-8859-9
                  #+ecl :iso-8859-9
                  #+abcl :iso-8859-9
                  #+allegro :iso8859-9
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-9) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-8859-9)))
  (let ((impl-enc #+sbcl :cp1254
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1254
                  #+ecl :windows-cp1254
                  #+abcl :|windows-1254|
                  #+allegro :1254
                  #+(and lispworks windows) '(win32:code-page :id 1254)))
    (is (dependent-name :cp1254) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1254))))

(subtest "russian"
  (let ((impl-enc #+sbcl :iso-8859-5
                  #+ccl :iso-8859-5
                  #+clisp charset:iso-8859-5
                  #+ecl :iso-8859-5
                  #+abcl :iso-8859-5
                  #+allegro :iso8859-5
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-5) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name  impl-enc):iso-8859-5)))
  (let ((impl-enc #+sbcl :koi8-r
                  #+ccl :cannot-treat
                  #+clisp charset:koi8-r
                  #+ecl :cannot-treat
                  #+abcl :koi8-r
                  #+allegro :koi8-r
                  #+lispworks :cannot-treat))
    (is (dependent-name :koi8-r) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :koi8-r)))
  (let ((impl-enc #+sbcl :koi8-u
                  #+ccl :cannot-treat
                  #+clisp charset:koi8-u
                  #+ecl :cannot-treat
                  #+abcl :koi8-u
                  #+allegro :cannot-treat
                  #+lispworks :cannot-treat))
    (is (dependent-name :koi8-u) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :koi8-u)))
  (let ((impl-enc #+sbcl :cp866
                  #+ccl :cannot-treat
                  #+clisp charset:cp866
                  #+ecl :dos-cp866
                  #+abcl :ibm866
                  #+allegro :cannot-treat
                  #+lispworks :cannot-treat))
    (is (dependent-name :cp866) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp866)))
  (let ((impl-enc #+sbcl :cp1251
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1251
                  #+ecl :windows-cp1251
                  #+abcl :|windows-1251|
                  #+allegro :1251
                  #+(and lispworks windows) '(win32:code-page :id 1251)))
    (is (dependent-name :cp1251) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1251))))

(subtest "polish"
  (let ((impl-enc #+sbcl :iso-8859-2
                  #+ccl :iso-8859-2
                  #+clisp charset:iso-8859-2
                  #+ecl :iso-8859-2
                  #+abcl :iso-8859-2
                  #+allegro :iso8859-2
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-2) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-8859-2)))
  (let ((impl-enc #+sbcl :cp1250
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1250
                  #+ecl :windows-cp1250
                  #+abcl :|windows-1250|
                  #+allegro :1250
                  #+(and lispworks windows) '(win32:code-page :id 1250)))
    (is (dependent-name :cp1250) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1250))))

(subtest "baltic"
  (let ((impl-enc #+sbcl :iso-8859-13
                  #+ccl :iso-8859-13
                  #+clisp charset:iso-8859-13
                  #+ecl :iso-8859-13
                  #+abcl :iso-8859-13
                  #+allegro :cannot-treat
                  #+lispworks :cannot-treat))
    (is (dependent-name :iso-8859-13) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :iso-8859-13)))
  (let ((impl-enc #+sbcl :cp1257
                  #+ccl :cannot-treat
                  #+clisp charset:windows-1257
                  #+ecl :windows-cp1257
                  #+abcl :|windows-1257|
                  #+allegro :1257
                  #+(and lispworks windows) '(win32:code-page :id 1257)))
    (is (dependent-name :cp1257) impl-enc)
    (unless (eq impl-enc :cannot-treat)
      (is (independent-name impl-enc) :cp1257))))

(subtest "end of line"
  (let ((impl-eol #+sbcl :cannot-treat
                  #+ccl :unix
                  #+clisp :unix
                  #+ecl :lf
                  #+abcl :lf
                  ;; https://franz.com/support/documentation/10.1/doc/operators/excl/eol-convention.htm
                  #+allegro :unix
                  #+lispworks :lf))
    (is (dependent-name :lf) impl-eol)
    (unless (eq impl-eol :cannot-treat)
      (is (independent-name impl-eol) :lf)))
  (let ((impl-eol #+sbcl :cannot-treat
                  #+ccl :macos
                  #+clisp :mac
                  #+ecl :cr
                  #+abcl :cr
                  #+allegro :mac
                  #+lispworks :cr))
    (is (dependent-name :cr) impl-eol)
    (unless (eq impl-eol :cannot-treat)
      (is (independent-name impl-eol) :cr)))
  (let ((impl-eol #+sbcl :cannot-treat
                  #+ccl :dos
                  #+clisp :dos
                  #+ecl :crlf
                  #+abcl :crlf
                  #+allegro :doc
                  #+lispworks :crlf))
    (is (dependent-name :crlf) impl-eol)
    (unless (eq impl-eol :cannot-treat)
      (is (independent-name impl-eol) :crlf))))

(subtest "names on flexi-streams"
  (subtest "unicode"
    (is (dependent-name :utf-8 :flexi-name) :utf-8)
    (is (independent-name :utf-8 :flexi-name) :utf-8)
    (is (dependent-name :ucs-2le :flexi-name) nil)
    (is (dependent-name :ucs-2be :flexi-name) nil)
    (is (dependent-name :utf-16 :flexi-name) :utf-16)
    (is (independent-name :utf-16 :flexi-name) :utf-16))

  (subtest "japanese"
    (is (dependent-name :iso-2022-jp :flexi-name) nil)
    (is (dependent-name :euc-jp :flexi-name) nil)
    (is (dependent-name :cp932 :flexi-name) nil))

  (subtest "taiwanese"
    (is (dependent-name :big5 :flexi-name) nil)
    (is (dependent-name :iso-2022-tw :flexi-name) nil))

  (subtest "chinese"
    (is (dependent-name :gb2312 :flexi-name) nil)
    (is (dependent-name :gb18030 :flexi-name) nil)
    (is (dependent-name :iso-2022-cn :flexi-name) nil))

  (subtest "korean"
    (is (dependent-name :euc-kr :flexi-name) nil)
    (is (dependent-name :johab :flexi-name) nil)
    (is (dependent-name :iso-2022-kr :flexi-name) nil))

  (subtest "arabic"
    (is (dependent-name :iso-8859-6 :flexi-name) :iso-8859-6)
    (is (independent-name :iso-8859-6 :flexi-name) :iso-8859-6)
    (is (dependent-name :cp1256 :flexi-name) nil))

  (subtest "greek"
    (is (dependent-name :iso-8859-7 :flexi-name) :iso-8859-7)
    (is (independent-name :iso-8859-7 :flexi-name) :iso-8859-7)
    (is (dependent-name :cp1253 :flexi-name) nil))

  (subtest "hebrew"
    (is (dependent-name :iso-8859-8 :flexi-name) :iso-8859-8)
    (is (independent-name :iso-8859-8 :flexi-name) :iso-8859-8)
    (is (dependent-name :cp1255 :flexi-name) nil))

  (subtest "turkish"
    (is (dependent-name :iso-8859-9 :flexi-name) :iso-8859-9)
    (is (independent-name :iso-8859-9 :flexi-name) :iso-8859-9)
    (is (dependent-name :cp1254 :flexi-name) nil))

  (subtest "russian"
    (is (dependent-name :iso-8859-5 :flexi-name) :iso-8859-5)
    (is (independent-name :iso-8859-5 :flexi-name) :iso-8859-5)
    (is (dependent-name :koi8-r :flexi-name) :koi8-r)
    (is (independent-name :koi8-r :flexi-name) :koi8-r)
    (is (dependent-name :koi8-u :flexi-name) nil)
    (is (dependent-name :cp866 :flexi-name) nil)
    (is (dependent-name :cp1251 :flexi-name) nil))

  (subtest "polish"
    (is (dependent-name :iso-8859-2 :flexi-name) :iso-8859-2)
    (is (independent-name :iso-8859-2 :flexi-name) :iso-8859-2)
    (is (dependent-name :cp1250 :flexi-name) nil))

  (subtest "baltic"
    (is (dependent-name :iso-8859-13 :flexi-name) :iso-8859-13)
    (is (independent-name :iso-8859-13 :flexi-name) :iso-8859-13)
    (is (dependent-name :cp1257 :flexi-name) nil))

  (subtest "eol"
    (is (dependent-name :lf :flexi-name) :lf)
    (is (independent-name :lf :flexi-name) :lf)
    (is (dependent-name :cr :flexi-name) :cr)
    (is (independent-name :cr :flexi-name) :cr)
    (is (dependent-name :crlf :flexi-name) :crlf)))
    (is (independent-name :crlf :flexi-name) :crlf)

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
