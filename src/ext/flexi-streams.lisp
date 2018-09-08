(in-package :cl-user)
(defpackage :inquisitor.ext.flexi-steams
  (:use :cl)
  (:import-from :inquisitor
                :dependent-name)
  (:import-from :inquisitor.external-format
                :%make-external-format))
(in-package :inquisitor.ext.flexi-steams)

(defun name-pos (independent-name)
  (position independent-name
            inquisitor.names::+name-mapping+
            :key (lambda (enc) (getf enc :name))))

(defun add-new-value (pos key value)
  (setf (nth pos inquisitor.names::+name-mapping+)
        (append (nth pos inquisitor.names::+name-mapping+)
                (list key value))))

;; Unicode
(add-new-value (name-pos :utf-8) :flexi-name :utf-8)
(add-new-value (name-pos :utf-16le) :flexi-name nil)
(add-new-value (name-pos :utf-16be) :flexi-name nil)
(add-new-value (name-pos :utf-16) :flexi-name :utf-16)

;; Japanese
(add-new-value (name-pos :iso-2022-jp) :flexi-name nil)
(add-new-value (name-pos :euc-jp) :flexi-name nil)
(add-new-value (name-pos :cp932) :flexi-name nil)

;; Taiwanese
(add-new-value (name-pos :big5) :flexi-name nil)
(add-new-value (name-pos :iso-2022-tw) :flexi-name nil)

;; Chinese
(add-new-value (name-pos :gb2312) :flexi-name nil)
(add-new-value (name-pos :gb18030) :flexi-name nil)
(add-new-value (name-pos :iso-2022-cn) :flexi-name nil)

;; Korean
(add-new-value (name-pos :euc-kr) :flexi-name nil)
(add-new-value (name-pos :johab) :flexi-name nil)
(add-new-value (name-pos :iso-2022-kr) :flexi-name nil)

;; Arabic
(add-new-value (name-pos :iso-8859-6) :flexi-name :iso-8859-6)
(add-new-value (name-pos ::cp1256) :flexi-name nil)

;; Greek
(add-new-value (name-pos :iso-8859-7) :flexi-name :iso-8859-7)
(add-new-value (name-pos :cp1253) :flexi-name nil)

;; Hebrew
(add-new-value (name-pos :iso-8859-8) :flexi-name :iso-8859-8)
(add-new-value (name-pos :cp1255) :flexi-name nil)

;; Turkish
(add-new-value (name-pos :iso-8859-9) :flexi-name :iso-8859-9)
(add-new-value (name-pos :cp1254) :flexi-name nil)

;; Russian
(add-new-value (name-pos :iso-8859-5) :flexi-name :iso-8859-5)
(add-new-value (name-pos :koi8-r) :flexi-name :koi8-r)
(add-new-value (name-pos :koi8-u) :flexi-name nil)
(add-new-value (name-pos :cp866) :flexi-name nil)
(add-new-value (name-pos :cp1251) :flexi-name nil)

;; Polish
(add-new-value (name-pos :iso-8859-2) :flexi-name :iso-8859-2)
(add-new-value (name-pos :cp1250) :flexi-name nil)

;; Baltic
(add-new-value (name-pos :iso-8859-13) :flexi-name :iso-8859-13)
(add-new-value (name-pos :cp1257) :flexi-name nil)

;; End of line markers
(add-new-value (name-pos :lf) :flexi-name :lf)
(add-new-value (name-pos :cr) :flexi-name :cr)
(add-new-value (name-pos :crlf) :flexi-name :crlf)

(defmethod %make-external-format
    ((type (eql :flexi)) enc eol &rest args &key &allow-other-keys)
  (let* ((enc-flexi (dependent-name enc :flexi))
         (eol-flexi (dependent-name eol :flexi))
         (args (append (list enc :eol-style eol)
                       (loop
                          :for (k v) :on args :by #'cddr
                          :unless (eq k :type)
                          :nconc (list k v)))))
    (apply #'flexi-streams:make-external-format args)))
