(in-package :cl-user)
(defpackage inquisitor.names
  (:nicknames :inq.names)
  (:export :available-encodings
           :available-eols
           :dependent-name
           :independent-name
           :unicode-p)
  (:use :cl))
(in-package :inquisitor.names)


(defvar +name-mapping+
  `(;;; unicode
    ((:utf8 . :unicode) .
     #+clisp ,charset:utf-8
     #-clisp :utf-8)
    ((:ucs-2le . :unicode) .
     #+clisp ,charset:unicode-16-little-endian
     #-clisp :utf-16le)
    ((:ucs-2be . :unicode) .
     #+clisp ,charset:unicode-16-big-endian
     #-clisp :utf-16be)
    ((:utf16 . :unicode) .
     #+clisp ,charset:utf-16
     #+ecl :utf-16
     #+ccl :utf-16
     #+abcl :utf-16
     #-(or clisp ecl ccl abcl) :cannot-treat)

    ;;; japanese
    ((:iso-2022-jp . :jp) .  ; jis
     ;; #+allegro :jis
     ;; #+lispworks :jis
     #+clisp ,charset:iso-2022-jp
     #+ecl :cannot-treat
     #+abcl :iso-2022-jp
     #-(or clisp ecl abcl) :cannot-treat)
    ((:euc-jp . :jp) .
     ;; #+lispworks :euc-jp
     #+clisp ,charset:euc-jp
     #+ecl :cannot-treat
     #-(or clisp ecl) :euc-jp)
    ((:cp932 . :jp) .
     ;; #+lispworks :sjis
     #+clisp ,charset:cp932
     #+ecl :windows-cp932
     #+sbcl :shift_jis
     #+ccl :windows-31j
     #+abcl :|x-MS932_0213|)

    ;;; taiwanese
    ((:big5 . :tw) .
     #+clisp ,charset:big5
     #+ecl :windows-cp950
     #+abcl :|Big5|
     #-(or clisp ecl abcl) :cannot-treat)
    ((:iso-2022-tw :tw) .
     #+clisp ,charset:euc-tw
     #+abcl :|x-EUC-TW|
     #-(or clisp abcl) :cannot-treat)

    ;;; chinese
    ((:gb2312 . :cn) .
     #+clisp ,charset:gbk
     #+ecl :windows-cp936
     #+sbcl :gbk
     #+ccl :cp936
     #+abcl :gbk)
    ((:gb18030 . :cn) .
     #+clisp ,charset:gb18030
     #+abcl :gb18030
     #-(or clisp abcl) :cannot-treat)
    ((:iso-2022-cn . :cn) .
     #+clisp ,charset:iso-2022-cn
     #+abcl :iso-2022-cn
     #-(or clisp abcl) :cannot-treat)

    ;;; korean
    ((:euc-kr . :kr) .
     #+clisp ,charset:euc-kr
     #+ecl :windows-cp949
     #+(or sbcl ccl) :cannot-treat
     #-(or clisp ecl sbcl ccl) :euc-kr)
    ((:johab . :kr) .
     #+clisp ,charset:johab
     #+abcl :|x-Johab|
     #+(or ecl sbcl ccl) :cannot-treat
     #-(or clisp ecl sbcl ccl abcl) :johab)
    ((:iso-2022-kr . :kr) .
     #+clisp ,charset:iso-2022-kr
     #+abcl :iso-2022-kr
     #-(or clisp abcl) :cannot-treat)

    ;;; arabic
    ((:iso-8859-6 . :ar) .
     #+clisp ,charset:iso-8859-6
     #-clisp :iso-8859-6)
    ((:cp1256 . :ar) .
     #+clisp ,charset:cp1256
     #+ecl :windows-cp1256
     #+ccl :cannot-treat
     #+abcl :|windows-1256|
     #-(or clisp ecl ccl abcl) :cp1256)

    ;;; greek
    ((:iso-8859-7 . :gr) .
     #+clisp ,charset:iso-8859-7
     #-clisp :iso-8859-7)
    ((:cp1253 . :gr) .
     #+clisp ,charset:cp1253
     #+ecl :windows-cp1253
     #+ccl :cannot-treat
     #+abcl  :|windows-1253|
     #-(or clisp ecl ccl abcl) :cp1253)

    ;;; hebrew
    ((:iso-8859-8 . :hw) .
     #+clisp ,charset:iso-8859-8
     #-clisp :iso-8859-8)
    ((:cp1255 . :hw) .
     #+clisp ,charset:cp1255
     #+ecl :windows-cp1255
     #+ccl :cannot-treat
     #+abcl :|windows-1255|
     #-(or clisp ecl ccl abcl) :cp1255)

    ;;; turkish
    ((:iso-8859-9 . :tr) .
     #+clisp ,charset:iso-8859-9
     #-clisp :iso-8859-9)
    ((:cp1254 . :tr) .
     #+clisp ,charset:cp1254
     #+ecl :windows-cp1254
     #+ccl :cannot-treat
     #+abcl :|windows-1254|
     #-(or clisp ecl ccl abcl) :cp1254)

    ;;; russian
    ((:iso-8859-5 . :ru) .
     #+clisp ,charset:iso-8859-5
     #-clisp :iso-8859-5)
    ((:koi8-r . :ru) .
     #+clisp ,charset:koi8-r
     #+sbcl :koi8-r
     #+(or ecl ccl) :cannot-treat
     #-(or clisp sbcl ecl ccl) :koi8-r)
    ((:koi8-u . :ru) .
     #+clisp ,charset:koi8-u
     #+(or ecl ccl) :cannot-treat
     #-(or clisp ecl ccl) :koi8-u)
    ((:cp866 . :ru) .
     #+clisp ,charset:cp866
     #+ecl :dos-cp866
     #+ccl :cannot-treat
     #+abcl :ibm866
     #-(or clisp ecl ccl abcl) :cp866)
    ((:cp1251 . :ru) .
     #+clisp ,charset:cp1251
     #+ecl :windows-cp1251
     #+ccl :cannot-treat
     #+abcl :|windows-1251|
     #-(or clisp ecl ccl abcl) :cp1251)

    ;;; polish
    ((:iso-8859-2 . :pl) .
     #+clisp ,charset:iso-8859-2
     #-clisp :iso-8859-2)
    ((:cp1250 . :pl) .
     #+clisp ,charset:cp1250
     #+ecl :windows-cp1250
     #+ccl :cannot-treat
     #+abcl :|windows-1250|
     #-(or clisp ecl ccl abcl) :cp1250)

    ;;; baltic
    ((:iso-8859-13 . :bl) .
     #+clisp ,charset:iso-8859-13
     #-clisp :iso-8859-13)
    ((:cp1257 . :bl) .
     #+clisp ,charset:cp1257
     #+ecl :windows-cp1257
     #+ccl :cannot-treat
     #+abcl :|windows-1257|
     #-(or clisp ecl ccl abcl) :cp1257)

    ;;; end of line
    ((:lf . :eol) .
     ;; #+lispworks :lf
     #+clisp :unix
     #+sbcl :cannot-treat
     #+ccl :unix
     #-(or clisp sbcl ccl) :lf)
    ((:cr . :eol) .
     ;; #+lispworks :cr
     #+clisp :mac
     #+sbcl :cannot-treat
     #+ccl :macos
     #-(or clisp sbcl ccl) :cr)
    ((:crlf . :eol) .
     ;; #+lispworks :crlf
     #+clisp :dos
     #+sbcl :cannot-treat
     #+ccl :dos
     #-(or clisp sbcl ccl) :crlf)))

(defun available-encodings ()
  (loop
     :for ((name . type) . impl-name) :in +name-mapping+
     :unless (eq type :eol)
     :collect name))

(defun available-eols ()
  (loop
     :for ((name . type) . impl-name) :in +name-mapping+
     :when (eq type :eol)
     :collect name))

(defun independent-name (dependent-name)
  (cdr (find-if (lambda (n) (eq dependent-name (caar n))) +name-mapping+)))

(defun dependent-name (independent-name)
  (caar (find-if (lambda (n) (eq independent-name (cdr n))) +name-mapping+)))

(defun unicode-p (encoding)
  (member encoding
          (loop
             :for ((name . type) . impl-name) :in +name-mapping+
             :when (eq type :unicode)
             :collect name)))
