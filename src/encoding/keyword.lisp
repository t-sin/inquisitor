#|
  This code is a part of inquisitor project
  and a derivate from guess (https://github.com/zqwell/guess).
|#
;;; This code is derivative of libguess-1.0 and guess-0.1.0 for common lisp.
;;; 
;;; Copyright (c) 2011 zqwell <zqwell@gmail.com>
;;; 
;;; The following is the original copyright notice.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;; 

(in-package :cl-user)
(defpackage inquisitor.encoding.keyword
  (:use :cl
        :cl-annot))
(in-package :inquisitor.encoding.keyword)

(enable-annot-syntax)



;;; eval-when is needed to compile generate-order
(eval-when (:compile-toplevel :load-toplevel :execute)
  @export
  (defun enc-name->keyword (enc)
    (funcall (symbol-function (find-symbol (string-upcase (format nil "~A-keyword" (symbol-name enc))) :inquisitor.encoding.keyword))))
  
;;;; japanese (:jp)
  @export
  (defun iso-2022-jp-keyword () ;; jis
    ;; #+allegro :jis
    ;; #+lispworks :jis
    #+clisp 'charset:iso-2022-jp
    #+ecl 'ext:iso-2022-jp
    #+sbcl (values :jis ; sbcl cannot treat JIS
                   :cannot-treat)
    #+ccl  (values :jis ; ccl cannot treat JIS
                   :cannot-treat)
    #+abcl :iso-2022-jp
    #-(or clisp ecl sbcl ccl abcl) :jis)

  ;; (defun jis-keyword ())

  @export
  (defun eucj-keyword ()
    ;; #+lispworks :euc-jp
    #+clisp 'charset:euc-jp
    #+ecl (values :euc-jp ; ecl cannot treat EUC-JP
                  :cannot-treat)
    ;; #+sbcl :euc-jp
    ;; #+ccl :euc-jp
    ;; #+abcl :euc-jp
    #-(or clisp ecl) :euc-jp)
  @export
  (defun sjis-keyword ()
    ;; #+lispworks :sjis
    #+clisp 'charset:shift-jis
    #+ecl 'ext:cp932
    ;; #+sbcl :sjis
    #+ccl :cp932
    #+abcl :|Shift_JIS|
    #-(or clisp ecl ccl) :sjis)
  @export
  (defun utf8-keyword ()
    ;; #+lispworks :utf-8
    #+clisp 'charset:utf-8
    ;; #+ecl :utf-8
    ;; #+sbcl :utf-8
    ;; #+ccl :utf-8
    ;; #+abcl :utf-8
    #-clisp :utf-8)

;;;; unicode
  @export
  (defun ucs-2le-keyword ()
    ;; #+lispworks :unicode ; default endian is :little-endian
    #+clisp 'charset:unicode-16-little-endian
    ;; #+ecl :ucs-2le
    ;; #+sbcl :ucs-2le
    ;; #+ccl :ucs-2le
    #+abcl :utf-16le
    #-(or clisp abcl) :ucs-2le)
  @export
  (defun ucs-2be-keyword ()
    ;; #+lispworks :unicode ; default endian is :little-endian
    #+clisp 'charset:unicode-16-big-endian
    ;; #+ecl :ucs-2be
    ;; #+sbcl :ucs-2be
    ;; #+ccl :ucs-2be
    #+abcl :utf-16be
    #-(or clisp abcl) :ucs-2be)
  @export
  (defun utf16-keyword ()
    ;; #+lispworks nil
    #+clisp 'charset:utf-16
    ;; #+ecl nil
    #+sbcl :utf-16be ; or :utf-16le
    #+ccl :utf-16
    #+abcl :utf-16
    #-(or clisp sbcl ccl) nil)

;;;; taiwanese (:tw)
  @export
  (defun big5-keyword () :big5)
  @export
  (defun iso-2022-tw-keyword () :iso-2022-tw)

;;;; chinese (:cn)
  @export
  (defun gb2312-keyword      () :gb2312)
  @export
  (defun gb18030-keyword     () :gb18030)
  @export
  (defun iso-2022-cn-keyword () :iso-2022-cn)
  
;;;; korean (:kr)
  @export
  (defun euck-keyword  () :euc-kr)
  @export
  (defun johab-keyword () :johab)
  @export
  (defun iso-2022-kr-keyword () :iso-2022-kr)
  
;;;; arabic (:ar)
  @export
  (defun iso8859-6-keyword () :iso8859-6)
  @export
  (defun cp1256-keyword    () :cp1256)

;;;; greek (:gr)
  @export
  (defun iso8859-7-keyword () :iso8859-7)
  @export
  (defun cp1253-keyword    () :cp1253)

;;;; hebrew (:hw)
  @export
  (defun iso8859-8-keyword () :iso8859-8)
  @export
  (defun cp1255-keyword    () :cp1255)

;;;; turkish (:tr)
  @export
  (defun iso8859-9-keyword () :iso8859-9)
  @export
  (defun cp1254-keyword    () :cp1254)

;;;; russian (:ru)
  @export
  (defun iso8859-5-keyword () :iso8859-5)
  @export
  (defun koi8-r-keyword    () :koi8-r)
  @export
  (defun koi8-u-keyword    () :koi8-u)
  @export
  (defun cp866-keyword     () :cp866)
  @export
  (defun cp1251-keyword    ()  :cp1251)

;;;; polish (:pl)
  @export
  (defun iso8859-2-keyword ()  :iso8859-2)
  @export
  (defun cp1250-keyword    ()  :cp1250)

;;;; baltic (:bl)
  @export
  (defun iso8859-13-keyword ()  :iso8859-13)
  @export
  (defun cp1257-keyword     ()  :cp1257)
  ) ;; eval-when
