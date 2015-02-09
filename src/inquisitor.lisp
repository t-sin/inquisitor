(in-package :cl-user)
(defpackage inquisitor
  (:use :cl
        :cl-annot)
  (:import-from :inquisitor.encoding.guess
                :ces-guess-from-vector
                :list-available-scheme)
  (:import-from :inquisitor.encoding.keyword
                :utf8-keyword
                :ucs-2le-keyword
                :ucs-2be-keyword
                :utf16-keyword)
  (:import-from :inquisitor.eol
                :eol-guess-from-vector))
(in-package :inquisitor)

(enable-annot-syntax)


@export
(defparameter *detecting-buffer-size* 1000)


(defmacro with-byte-array ((var dim) &body body)
  `(let ((,var (make-array ,dim
                           :element-type '(unsigned-byte 8))))
     ,@body))

(defun byte-array-p (vec)
  (and (typep vec 'vector)
       (equal (array-element-type vec) '(unsigned-byte 8))))

(defun byte-input-stream-p (stream)
  (and (typep stream 'stream)
       (input-stream-p stream)
       (equal (stream-element-type stream) '(unsigned-byte 8))))


@export
(defun make-external-format (enc eol)
  #+clisp (ext:make-encoding :charset enc
                             :line-terminator eol)
  #+ecl `(,enc ,eol)
  #+sbcl enc
  #+ccl (ccl:make-external-format :character-encoding enc
                                  :line-termination eol)
  #+abcl `(,enc :eol-style ,eol)
  #-(or clisp ecl sbcl ccl abcl) (error "your implementation is not supported."))


@export
(defun detect-encoding (stream scheme)
  (when (byte-input-stream-p stream)
    (with-byte-array (vec *detecting-buffer-size*)
      (read-sequence vec stream)
      (ces-guess-from-vector vec scheme))))

@export
(defun detect-end-of-line (stream)
  (when (byte-input-stream-p stream)
    (with-byte-array (vec *detecting-buffer-size*)
      (read-sequence vec stream)
      (eol-guess-from-vector vec))))
