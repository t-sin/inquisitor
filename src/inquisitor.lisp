(in-package :cl-user)
(defpackage inquisitor
  (:use :cl)
  (:import-from :inquisitor.encoding.guess
                :ces-guess-from-vector
                :list-available-scheme)
  (:import-from :inquisitor.encoding.keyword
                :utf8-keyword
                :ucs-2le-keyword
                :ucs-2be-keyword
                :utf16-keyword)
  (:import-from :inquisitor.eol
                :eol-guess-from-vector)
  (:import-from :alexandria
                :type=)
  (:import-from :metabang-bind
                :bind)
  (:export :*detecting-buffer-size*
           :unicode-p
           :make-external-format
           :detect-encoding
           :detect-end-of-line
           :detect-external-format))
(in-package :inquisitor)


(defparameter *detecting-buffer-size* 1000)


(defmacro with-byte-array ((var dim) &body body)
  `(let ((,var (make-array ,dim
                           :element-type '(unsigned-byte 8))))
     ,@body))

(defun byte-array-p (vec)
  (and (typep vec 'vector)
       (type= (array-element-type vec) '(unsigned-byte 8))))

(defun byte-input-stream-p (stream)
  (and (typep stream 'stream)
       (input-stream-p stream)
       (type= (stream-element-type stream) '(unsigned-byte 8))))


(defun unicode-p (encoding)
  (member encoding
          (list (utf8-keyword)
                (ucs-2le-keyword)
                (ucs-2be-keyword)
                (utf16-keyword))))

(defun make-external-format (enc eol)
  #+clisp (ext:make-encoding :charset enc
                             :line-terminator eol)
  #+ecl `(,enc ,eol)
  #+sbcl enc
  #+ccl (ccl:make-external-format :character-encoding enc
                                  :line-termination eol)
  #+abcl `(,enc :eol-style ,eol)
  #-(or clisp ecl sbcl ccl abcl) (error "your implementation is not supported."))


(defun detect-encoding (stream scheme)
  (when (byte-input-stream-p stream)
    (with-byte-array (vec *detecting-buffer-size*)
      (read-sequence vec stream)
      (ces-guess-from-vector vec scheme))))

(defun detect-end-of-line (stream)
  (when (byte-input-stream-p stream)
    (with-byte-array (vec *detecting-buffer-size*)
      (read-sequence vec stream)
      (eol-guess-from-vector vec))))


(defmethod detect-external-format ((vec vector) (scheme symbol))
  (if (byte-array-p vec)
      (bind (((:values enc enc-ct) (ces-guess-from-vector vec scheme))
             ((:values eol eol-ct) (eol-guess-from-vector vec)))
        (if enc-ct
            (error (format nil "unsupported on ~a: ~{~a~^, ~}"
                           (lisp-implementation-type) enc))
            (make-external-format enc eol)))
      (error (format nil "supplied vector is not a byte array."))))

(defmethod detect-external-format ((stream stream) (scheme symbol))
  (when (byte-input-stream-p stream)
    (with-byte-array (vec *detecting-buffer-size*)
      (read-sequence vec stream)
      (detect-external-format vec scheme))))

(defmethod detect-external-format ((path pathname) (scheme symbol))
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-external-format in scheme)))
  
