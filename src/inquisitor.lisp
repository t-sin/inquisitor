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


(defparameter *detecting-buffer-size* 1000)

(defgeneric detect-encoding (input symbol))
(defgeneric detect-end-of-line (input))
(defgeneric detect-external-format (input symbol))

(defmethod detect-encoding ((stream stream) (scheme symbol))
  (if (byte-input-stream-p stream)
      (if (file-position-changable-p stream)
          (let ((pos (file-position stream)))
            (with-byte-array (vec *detecting-buffer-size*)
              (read-sequence vec stream)
              (prog1
                  (ces-guess-from-vector vec scheme)
                (file-position stream pos))))
          (error (format nil "supplied stream is not file-position changable.")))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-encoding ((path pathname) (scheme symbol))
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
      (let (enc enc-ct eol)
        (setf (values enc enc-ct) (ces-guess-from-vector vec scheme))
        (setf eol (eol-guess-from-vector vec))
        (if enc-ct
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
  (detect-external-format-from-file path scheme nil))

(defun detect-external-format-from-file (path scheme &optional all-scan-p)
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (if all-scan-p
        (let ((*detecting-buffer-size* (file-length in)))
          (detect-external-format in scheme))
        (detect-external-format in scheme))))
