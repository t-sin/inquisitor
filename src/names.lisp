(in-package :cl-user)
(defpackage inquisitor.names
  (:nicknames :inq.names)
  (:export :+available-encodings+
           :+available-eols+
           :dependent-name
           :independent-name
           :unicode-p)
  (:use :cl))
(in-package :inquisitor.names)


(defvar +name-mapping+
  `(;;; unicode
    ((:utf-8 . :unicode) .
     #+clisp ,charset:utf-8
     #+allegro :utf8
     #-(or clisp allegro) :utf-8)
    ((:ucs-2le . :unicode) .
     #+clisp ,charset:unicode-16-little-endian
     #+allegro :cannot-treat
     #+lispworks '(unicode :little-endian)
     #-(or clisp allegro lispworks) :utf-16le)
    ((:ucs-2be . :unicode) .
     #+clisp ,charset:unicode-16-big-endian
     #+allegro :cannot-treat
     #+lispworks '(unicode :big-endian)
     #-(or clisp allegro lispworks) :utf-16be)
    ((:utf-16 . :unicode) .
     #+clisp ,charset:utf-16
     #+ecl :utf-16
     #+ccl :utf-16
     #+abcl :utf-16
     #-(or clisp ecl ccl abcl) :cannot-treat)

    ;;; japanese
    ((:iso-2022-jp . :jp) .  ; jis
     ;; #+lispworks :jis
     #+clisp ,charset:iso-2022-jp
     #+ecl :cannot-treat
     #+abcl :iso-2022-jp
     #+allegro :jis
     #+lispworks :jis
     #-(or clisp ecl abcl allegro lispworks) :cannot-treat)
    ((:euc-jp . :jp) .
     ;; #+lispworks :euc-jp
     #+clisp ,charset:euc-jp
     #+ecl :cannot-treat
     #+allegro :euc
     #+lispworks :euc-jp
     #-(or clisp ecl allegro lispworks) :euc-jp)
    ((:cp932 . :jp) .
     ;; #+lispworks :sjis
     #+clisp ,charset:cp932
     #+ecl :windows-cp932
     #+sbcl :shift_jis
     #+ccl :windows-31j
     #+abcl :|x-MS932_0213|
     #+allegro :shiftjis
     #+lispworks :sjis)

    ;;; taiwanese
    ((:big5 . :tw) .
     #+clisp ,charset:big5
     #+ecl :windows-cp950
     #+abcl :|Big5|
     #+allegro :big5
     #-(or clisp ecl abcl allegro) :cannot-treat)
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
     #+abcl :gbk
     #+allegro :cannot-treat
     #+lispworks :gbk)
    ((:gb18030 . :cn) .
     #+clisp ,charset:gb18030
     #+abcl :gb18030
     #+allegro :gb18030
     #-(or clisp abcl allegro) :cannot-treat)
    ((:iso-2022-cn . :cn) .
     #+clisp ,charset:iso-2022-cn
     #+abcl :iso-2022-cn
     #-(or clisp abcl) :cannot-treat)

    ;;; korean
    ((:euc-kr . :kr) .
     #+clisp ,charset:euc-kr
     #+ecl :windows-cp949
     #+allegro :949
     #+(and lispworks windows) '(win32:code-page :id 949)
     #+(or sbcl ccl) :cannot-treat
     #-(or clisp ecl sbcl ccl allegro) :euc-kr)
    ((:johab . :kr) .
     #+clisp ,charset:johab
     #+abcl :|x-Johab|
     #+(or ecl sbcl ccl allegro lispworks) :cannot-treat
     #-(or clisp ecl sbcl ccl abcl allegro) :johab)
    ((:iso-2022-kr . :kr) .
     #+clisp ,charset:iso-2022-kr
     #+abcl :iso-2022-kr
     #-(or clisp abcl) :cannot-treat)

    ;;; arabic
    ((:iso-8859-6 . :ar) .
     #+clisp ,charset:iso-8859-6
     #+allegro :iso8859-6
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-6)
    ((:cp1256 . :ar) .
     #+clisp ,charset:cp1256
     #+ecl :windows-cp1256
     #+ccl :cannot-treat
     #+abcl :|windows-1256|
     #+allegro :1256
     #+(and lispworks windows) '(win32:code-page :id 1256)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1256)

    ;;; greek
    ((:iso-8859-7 . :gr) .
     #+clisp ,charset:iso-8859-7
     #+allegro :iso8859-7
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-7)
    ((:cp1253 . :gr) .
     #+clisp ,charset:cp1253
     #+ecl :windows-cp1253
     #+ccl :cannot-treat
     #+abcl  :|windows-1253|
     #+allegro :1253
     #+(and lispworks windows) '(win32:code-page :id 1253)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1253)

    ;;; hebrew
    ((:iso-8859-8 . :hw) .
     #+clisp ,charset:iso-8859-8
     #+allegro :iso8559-8
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-8)
    ((:cp1255 . :hw) .
     #+clisp ,charset:cp1255
     #+ecl :windows-cp1255
     #+ccl :cannot-treat
     #+abcl :|windows-1255|
     #+allegro :1255
     #+(and lispworks windows) '(win32:code-page :id 1255)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1255)

    ;;; turkish
    ((:iso-8859-9 . :tr) .
     #+clisp ,charset:iso-8859-9
     #+allegro :iso8859-9
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-9)
    ((:cp1254 . :tr) .
     #+clisp ,charset:cp1254
     #+ecl :windows-cp1254
     #+ccl :cannot-treat
     #+abcl :|windows-1254|
     #+allegro :1254
     #+(and lispworks windows) '(win32:code-page :id 1254)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1254)

    ;;; russian
    ((:iso-8859-5 . :ru) .
     #+clisp ,charset:iso-8859-5
     #+allegro :iso8859-5
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-5)
    ((:koi8-r . :ru) .
     #+clisp ,charset:koi8-r
     #+sbcl :koi8-r
     #+(or ecl ccl lispworks) :cannot-treat
     #-(or clisp sbcl ecl ccl) :koi8-r)
    ((:koi8-u . :ru) .
     #+clisp ,charset:koi8-u
     #+(or ecl ccl allegro lispworks) :cannot-treat
     #-(or clisp ecl ccl allegro lispworks) :koi8-u)
    ((:cp866 . :ru) .
     #+clisp ,charset:cp866
     #+ecl :dos-cp866
     #+ccl :cannot-treat
     #+abcl :ibm866
     #+allegro :cannot-treat
     #+lispworks :cannot-treat
     #-(or clisp ecl ccl abcl lispworks) :cp866)
    ((:cp1251 . :ru) .
     #+clisp ,charset:cp1251
     #+ecl :windows-cp1251
     #+ccl :cannot-treat
     #+abcl :|windows-1251|
     #+allegro :1251
     #+(and lispworks windows) '(win32:code-page :id 1251)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1251)

    ;;; polish
    ((:iso-8859-2 . :pl) .
     #+clisp ,charset:iso-8859-2
     #+allegro :iso8859-2
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-2)
    ((:cp1250 . :pl) .
     #+clisp ,charset:cp1250
     #+ecl :windows-cp1250
     #+ccl :cannot-treat
     #+abcl :|windows-1250|
     #+allegro :1250
     #+(and lispworks windows) '(win32:code-page :id 1250)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1250)

    ;;; baltic
    ((:iso-8859-13 . :bl) .
     #+clisp ,charset:iso-8859-13
     #+allegro :cannot-treat
     #+lispworks :cannot-treat
     #-(or clisp allegro lispworks) :iso-8859-13)
    ((:cp1257 . :bl) .
     #+clisp ,charset:cp1257
     #+ecl :windows-cp1257
     #+ccl :cannot-treat
     #+abcl :|windows-1257|
     #+allegro :1257
     #+(and lispworks windows) '(win32:code-page :id 1257)
     #-(or clisp ecl ccl abcl allegro lispworks) :cp1257)

    ;;; end of line
    ((:lf . :eol) .
     #+clisp :unix
     #+sbcl :cannot-treat
     #+ccl :unix
     #+allegro :unix
     #+lispworks :lf
     #-(or clisp sbcl ccl allegro lispworks) :lf)
    ((:cr . :eol) .
     #+clisp :mac
     #+sbcl :cannot-treat
     #+ccl :macos
     #+allegro :mac
     #+lispworks :cr
     #-(or clisp sbcl allegro lispworks) :cr)
    ((:crlf . :eol) .
     ;; #+lispworks :crlf
     #+clisp :dos
     #+sbcl :cannot-treat
     #+ccl :dos
     #+allegro :dos
     #+lispworks :crlf
     #-(or clisp sbcl ccl allegro lispworks) :crlf)))

(defvar +available-encodings+
  (loop
     :for ((name . type) . impl-name) :in +name-mapping+
     :unless (eq type :eol)
     :collect name))

(defvar +available-eols+
  (loop
     :for ((name . type) . impl-name) :in +name-mapping+
     :when (eq type :eol)
     :collect name))

(defun independent-name (dependent-name)
  (caar (find-if (lambda (n) (eql dependent-name (cdr n))) +name-mapping+)))

(defun dependent-name (independent-name)
  (cdr (find-if (lambda (n) (eql independent-name (caar n))) +name-mapping+)))

(defun unicode-p (encoding)
  (member encoding
          (loop
             :for ((name . type) . impl-name) :in +name-mapping+
             :when (eq type :unicode)
             :collect name)))
