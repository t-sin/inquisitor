(in-package :cl-user)
(defpackage inquisitor
  (:nicknames :inq)
  (:use :cl)
  (:import-from :inquisitor.encoding.guess
                :ces-guess-from-vector
                :list-available-scheme)
  (:import-from :inquisitor.eol
                :eol-available-p
                :eol-guess-from-vector)
  (:import-from :inquisitor.external-format
                :make-external-format)
  (:import-from :inquisitor.names
                :+available-encodings+
                :+available-eols+
                :dependent-name
                :independent-name
                :unicode-p)
  (:import-from :inquisitor.util
                :with-byte-array
                :byte-array-p
                :byte-input-stream-p
                :file-position-changable-p)
  (:export :*detecting-buffer-size*
           :make-external-format
           :list-available-scheme
           :eol-available-p
           :+available-encodings+
           :+available-eols+
           :independent-name
           :dependent-name
           :unicode-p
           :detect-encoding
           :detect-end-of-line
           :detect-external-format
           :detect-external-format-from-file))
(in-package :inquisitor)


(defparameter *default-buffer-size* 1000
  "Specifies default buffer size is consed and used by `dedect-encoding`,
`detect-end-of-line` and `detect-external-format`.")


(defgeneric detect-encoding (input symbol))

(defmethod detect-encoding ((stream stream) (scheme symbol))
  "Detect character encoding scheme under the `scheme` from `stream`. Note that this
method modifies `stream`'s file position."
  (if (byte-input-stream-p stream)
      (let* ((buffer-length *default-buffer-size*)
             (buffer (make-array buffer-length :element-type '(unsigned-byte 8)))
             (order)
             (encoding))
        (loop
           :for num-read := (read-sequence buffer stream)
           :if (< num-read buffer-length)
           :do (return-from detect-encoding
                 (ces-guess-from-vector (subseq buffer 0 num-read) scheme))
           :else
           :do (multiple-value-bind (enc ord)
                   (ces-guess-from-vector (subseq buffer 0 num-read) scheme)
                 (setf encoding enc
                       order ord)))
        encoding)
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-encoding ((path pathname) (scheme symbol))
  "Detect character encoding scheme under the `scheme` from `pathname`."
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-encoding in scheme)))

(defmethod detect-end-of-line ((stream stream))
  (if (byte-input-stream-p stream)
      (if (file-position-changable-p stream)
          (let ((pos (file-position stream)))
            (with-byte-array (vec 500)
              (prog1
                  (loop for n = (read-sequence vec stream)
                     for eol = (eol-guess-from-vector vec)
                     until (or (zerop n)
                               (not (null eol)))
                     finally (return eol))
                (file-position stream pos))))
          (error (format nil "supplied stream is not file-position changable.")))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-end-of-line ((path pathname))
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-end-of-line in)))

(defmethod detect-external-format ((vec vector) (scheme symbol))
  (if (byte-array-p vec)
      (let (enc eol)
        (multiple-value-bind (encoding order)
            (ces-guess-from-vector vec scheme)
          (declare (ignore order))
          (setf enc (dependent-name encoding)))
        (setf eol (eol-guess-from-vector vec))
        (if (eq enc :cannot-treat)
            (error (format nil "unsupported on ~a: ~{~a~^, ~}"
                           (lisp-implementation-type) enc))
            (if (null eol)
                (make-external-format enc :lf)
                (make-external-format enc eol))))
      (error (format nil "supplied vector is not a byte array."))))

(defmethod detect-external-format ((stream stream) (scheme symbol))
  (if (byte-input-stream-p stream)
      (if (file-position-changable-p stream)
          (let ((pos (file-position stream)))
            (with-byte-array (vec *detecting-buffer-size*)
              (read-sequence vec stream)
              (prog1
                  (detect-external-format vec scheme)
                (file-position stream pos))))
          (error (format nil "supplied stream is not file-position changable.")))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-external-format ((path pathname) (scheme symbol))
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-external-format in scheme)))
