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

(subtest "unicode")

(subtest "japanese")

(subtest "tiwanese")

(subtest "chinese")

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
