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
  `(

;;; Unicode character set

    (:name :utf-8
     :type :unicode
     :impl-name
     #+sbcl :utf-8
     #+ccl :utf-8
     #+clisp ,charset:utf-8
     #+ecl :utf-8
     #+abcl :utf-8
     #+lispworks :utf-8
     #+allegro :utf8)

    (:name :utf-16le
     :type :unicode
     :impl-name
     #+sbcl :utf-16le
     #+ccl :utf-16le
     #+clisp ,charset:unicode-16-little-endian
     #+ecl :utf-16le
     #+abcl :utf-16le
     ;; LispWorks supports Unicode which range on BMP
     ;; http://www.lispworks.com/documentation/lw61/LW/html/lw-498.htm
     #+lispworks '(unicode :little-endian)
     #+allegro :cannot-treat)

    (:name :utf-16be
     :type :unicode
     :impl-name
     #+sbcl :utf-16be
     #+ccl :utf-16be
     #+clisp ,charset:unicode-16-big-endian
     #+ecl :utf-16be
     #+abcl :utf-16be
     #+lispworks '(unicode :big-endian)
     #+allegro :cannot-treat)

    (:name :utf-16
     :type :unicode
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :utf-16
     #+clisp ,charset:utf-16
     #+ecl :utf-16
     #+abcl :utf-16
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

;;; Japanese

    (:name :iso-2022-jp
     :type :jp
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:iso-2022-jp
     #+ecl :cannot-treat
     #+abcl :iso-2022-jp
     #+lispworks :jis
     #+allegro :jis)

    (:name :euc-jp
     :type :jp
     :impl-name
     #+sbcl :euc-jp
     #+ccl :euc-jp
     #+clisp ,charset:euc-jp
     #+ecl :cannot-treat
     #+abcl :euc-jp
     #+lispworks :euc-jp
     #+allegro :euc)

    (:name :cp932
     :type :jp
     :impl-name
     #+sbcl :shift_jis
     #+ccl :windows-31j
     #+clisp ,charset:cp932
     #+ecl :windows-cp932
     #+abcl :|x-MS932_0213|
     #+lispworks :sjis
     #+allegro :shiftjis)

;;; Taiwanese

    (:name :big5
     :type :tw
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:big5
     #+ecl :windows-cp950
     #+abcl :|Big5|
     #+lispworks :cannot-treat
     #+allegro :big5)

    (:name :iso-2022-tw
     :type :tw
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:euc-tw
     #+ecl :cannot-treat
     #+abcl :|x-EUC-TW|
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

;;; Chinese

    (:name :gb2312
     :type :cn
     :impl-name
     #+sbcl :gbk
     #+ccl :cp936
     #+clisp ,charset:gbk
     #+ecl :windows-cp936
     #+abcl :gbk
     #+lispworks :gbk
     #+allegro :cannot-treat)

    (:name :gb18030
     :type :cn
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:gb18030
     #+ecl :cannot-treat
     #+abcl :gb18030
     #+lispworks :cannot-treat
     #+allegro :gb18030)

    (:name :iso-2022-cn
     :type :cn
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:iso-2022-cn
     #+ecl :cannot-treat
     #+abcl :iso-2022-cn
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

;;; Korean

    (:name :euc-kr
     :type :kr
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:euc-kr
     #+ecl :windows-cp949
     #+abcl :euc-kr
     #+lispworks (progn #+win32 '(win32:code-page :id 949) #-win32 :cannot-treat)
     #+allegro :949)

    (:name :johab
     :type :kr
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:johab
     #+ecl :cannot-treat
     #+abcl :|x-Johab|
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

    (:name :iso-2022-kr
     :type :kr
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :cannot-treat
     #+clisp ,charset:iso-2022-kr
     #+ecl :cannot-treat
     #+abcl :iso-2022-kr
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

;;; Arabic

    (:name :iso-8859-6
     :type :ar
     :impl-name
     #+sbcl :iso-8859-6
     #+ccl :iso-8859-6
     #+clisp ,charset:iso-8859-6
     #+ecl :iso-8859-6
     #+abcl :iso-8859-6
     #+lispworks :cannot-treat
     #+allegro :iso8859-6)

    (:name :cp1256
     :type :ar
     :impl-name
     #+sbcl :cp1256
     #+ccl :cannot-treat
     #+clisp ,charset:cp1256
     #+ecl :windows-cp1256
     #+abcl :|windows-1256|
     #+lispworks (progn #+win32 '(win32:code-page :id 1256) #-win32 :cannot-treat)
     #+allegro :1256)

;;; Greek

    (:name :iso-8859-7
     :type :gr
     :impl-name
     #+sbcl :iso-8859-7
     #+ccl :iso-8859-7
     #+clisp ,charset:iso-8859-7
     #+ecl :iso-8859-7
     #+abcl :iso-8859-7
     #+lispworks :cannot-treat
     #+allegro :iso8859-7)

    (:name :cp1253
     :type :gr
     :impl-name
     #+sbcl :cp1253
     #+ccl :cannot-treat
     #+clisp ,charset:cp1253
     #+ecl :windows-cp1253
     #+abcl :|windows-1253|
     #+lispworks (progn #+win32 '(win32:code-page :id 1253) #-win32 :cannot-treat)
     #+allegro :1253)

;;; Hebrew

    (:name :iso-8859-8
     :type :hw
     :impl-name
     #+sbcl :iso-8859-8
     #+ccl :iso-8859-8
     #+clisp :iso-8859-8
     #+ecl :iso-8859-8
     #+abcl :iso-8859-8
     #+lispworks :cannot-treat
     #+allegro :iso8559-8)

    (:name :cp1255
     :type :hw
     :impl-name
     #+sbcl :cp1255
     #+ccl :cannot-treat
     #+clisp ,charset:cp1255
     #+ecl :windows-cp1255
     #+abcl :|windows-1255|
     #+lispworks (progn #+win32 '(win32:code-page :id 1255) #-win32 :cannot-treat)
     #+allegro :1255)

;;; Turkish

    (:name :iso-8859-9
     :type :tr
     :impl-name
     #+sbcl :iso-8859-9
     #+ccl :iso-8859-9
     #+clisp ,charset:iso-8859-9
     #+ecl :iso-8859-9
     #+abcl :iso-8859-9
     #+lispworks :cannot-treat
     #+allegro :iso8859-9)

    (:name :cp1254
     :type :tr
     :impl-name
     #+sbcl :cp1254
     #+ccl :cannot-treat
     #+clisp ,charset:cp1254
     #+ecl :windows-cp1254
     #+abcl :|windows-1254|
     #+lispworks (progn #+win32 '(win32:code-page :id 1254) #-win32 :cannot-treat)
     #+allegro :1254)

;;; Russian

    (:name :iso-8859-5
     :type :ru
     :impl-name
     #+sbcl :iso-8859-5
     #+ccl :iso-8859-5
     #+clisp ,charset:iso-8859-5
     #+ecl :iso-8859-5
     #+abcl :iso-8859-5
     #+lispworks :cannot-treat
     #+allegro :iso8859-5)

    (:name :koi8-r
     :type :ru
     :impl-name
     #+sbcl :koi8-r
     #+ccl :cannot-treat
     #+clisp ,charset:koi8-r
     #+ecl :cannot-treat
     #+abcl :koi8-r
     #+lispworks :cannot-treat
     #+allegro :koi8-r)

    (:name :koi8-u
     :type :ru
     :impl-name
     #+sbcl :koi8-u
     #+ccl :cannot-treat
     #+clisp ,charset:koi8-u
     #+ecl :cannot-treat
     #+abcl :koi8-u
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

    (:name :cp866
     :type :ru
     :impl-name
     #+sbcl :cp866
     #+ccl :cannot-treat
     #+clisp ,charset:cp866
     #+ecl :dos-cp866
     #+abcl :ibm866
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

    (:name :cp1251
     :type :ru
     :impl-name
     #+sbcl :cp1251
     #+ccl :cannot-treat
     #+clisp ,charset:cp1251
     #+ecl :windows-cp1251
     #+abcl :|windows-1251|
     #+lispworks (progn #+win32 '(win32:code-page :id 1251) #-win32 :cannot-treat)
     #+allegro :1251)

;;; Polish

    (:name :iso-8859-2
     :type :pl
     :impl-name
     #+sbcl :iso-8859-2
     #+ccl :iso-8859-2
     #+clisp ,charset:iso-8859-2
     #+ecl :iso-8859-2
     #+abcl :iso-8859-2
     #+lispworks :cannot-treat
     #+allegro :iso8859-2)

    (:name :cp1250
     :type :pl
     :impl-name
     #+sbcl :cp1250
     #+ccl :cannot-treat
     #+clisp ,charset:cp1250
     #+ecl :windows-cp1250
     #+abcl :|windows-1250|
     #+lispworks (progn #+win32 '(win32:code-page :id 1250) #-win32 :cannot-treat)
     #+allegro :1250)

;;; Baltic

    (:name :iso-8859-13
     :type :bl
     :impl-name
     #+sbcl :iso-8859-13
     #+ccl :iso-8859-13
     #+clisp ,charset:iso-8859-13
     #+ecl :iso-8859-13
     #+abcl :iso-8859-13
     #+lispworks :cannot-treat
     #+allegro :cannot-treat)

    (:name :cp1257
     :type :bl
     :impl-name
     #+sbcl :cp1257
     #+ccl :cannot-treat
     #+clisp ,charset:cp1257
     #+ecl :windows-cp1257
     #+abcl :|windows-1257|
     #+lispworks (progn #+win32 '(win32:code-page :id 1257) #-win32 :cannot-treat)
     #+allegro :1257)

;;; End of line markers

    (:name :lf
     :type :eol
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :unix
     #+clisp :unix
     #+ecl :lf
     #+abcl :lf
     #+lispworks :lf
     #+allegro :unix)

    (:name :cr
     :type :eol
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :macos
     #+clisp :mac
     #+ecl :cr
     #+abcl :cr
     #+lispworks :cr
     #+allegro :mac)

    (:name :crlf
     :type :eol
     :impl-name
     #+sbcl :cannot-treat
     #+ccl :dos
     #+clisp :dos
     #+ecl :crlf
     #+abcl :crlf
     #+lispworks :crlf
     #+allegro :dos)))

(defvar +available-encodings+
  (loop
     :for enc :in +name-mapping+
     :for name := (getf enc :name)
     :for type := (getf enc :type)
     :unless (eq type :eol)
     :collect name))

(defvar +available-eols+
  (loop
     :for enc :in +name-mapping+
     :for name := (getf enc :name)
     :for type := (getf enc :type)
     :when (eq type :eol)
     :collect name))

(defun independent-name (dependent-name &optional type)
  (getf (find-if (lambda (enc)
                   (let ((impl-name (getf enc (if type type :impl-name))))
                     (and (not (eq impl-name :cannot-treat))
                          (eq impl-name dependent-name))))
                 +name-mapping+)
        :name))

(defun dependent-name (independent-name &optional type)
  (let ((encoding (find-if (lambda (enc) (eq (getf enc :name) independent-name))
                           +name-mapping+)))
    (if type
        (getf encoding type)
        (getf encoding :impl-name))))

(defun unicode-p (independent-name)
  (member independent-name
          (loop
             :for enc :in +name-mapping+
             :for name := (getf enc :name)
             :for type := (getf enc :type)
             :when (eq type :unicode)
             :collect name)))
