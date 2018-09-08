(in-package :cl-user)
(defpackage :inquisitor-test.ext.flexi-streams
  (:use :cl
        :inquisitor.external-format
        :inquisitor.names
        :prove))
(in-package :inquisitor-test.ext.flexi-streams)

(plan 2)

(subtest "names on flexi-streams"
  (subtest "unicode"
    (is (dependent-name :utf-8 :flexi-name) :utf-8)
    (is (independent-name :utf-8 :flexi-name) :utf-8)
    (is (dependent-name :utf-16le :flexi-name) nil)
    (is (dependent-name :utf-16be :flexi-name) nil)
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
    (is (dependent-name :crlf :flexi-name) :crlf)
    (is (independent-name :crlf :flexi-name) :crlf)))

(subtest "make external-format for flexi-streams"
  (let* ((utf-8 (dependent-name :utf-8 :flexi-name))
         (lf (dependent-name :lf :flexi-name))
         (ef (make-external-format utf-8 lf :type :flexi :little-endian nil)))
    (is (slot-value ef 'flex::name) :utf-8)
    (is (slot-value ef 'flex::eol-style) :lf)
    (is (slot-value ef 'flex::little-endian) nil)))

(finalize)
