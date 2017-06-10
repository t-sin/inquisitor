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
  "Detect end-of-line style from `stream`. Note that this method modifies `stream`'s
file position."
  (if (byte-input-stream-p stream)
      (let* ((buffer-length *default-buffer-size*)
             (buffer (make-array buffer-length :element-type '(unsigned-byte 8))))
        (loop
           :for num-read := (read-sequence buffer stream)
           :if (< num-read buffer-length)
           :do (return-from detect-end-of-line
                 (eol-guess-from-vector (subseq buffer 0 num-read)))
           :else
           :do (let ((eol (eol-guess-from-vector buffer)))
                 (when eol
                   (return-from detect-end-of-line eol)))))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-end-of-line ((path pathname))
    "Detect end-of-line style from `pathname`."
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-end-of-line in)))


(defgeneric detect-external-format (input symbol))

(defmethod detect-external-format ((buffer vector) (scheme symbol))
  "Detect external-format under the `scheme` from `buffer`."
  (if (byte-array-p buffer)
      (let* ((enc (ces-guess-from-vector buffer scheme))
             (eol (eol-guess-from-vector buffer))
             (enc-impl (dependent-name enc))
             (eol-impl (dependent-name eol)))
        (if (or (eq enc-impl :cannot-treat)
                (eq eol-impl :cannot-treat))
            (values nil (list enc eol))
            (values
             (if eol-impl
                 (make-external-format enc-impl eol-impl)
                 (make-external-format enc-impl :lf))
             (list enc eol))))
      (error (format nil "supllied vector is not a byte array."))))

(defmethod detect-external-format ((stream stream) (scheme symbol))
  "Detect external-format under the `scheme` from `buffer`. Note that this method
method modifies `stream`'s file position."
  (if (byte-input-stream-p stream)
      (let* ((buffer-length *default-buffer-size*)
             (buffer (make-array buffer-length :element-type '(unsigned-byte 8)))
             (encoding)
             (order)
             (end-of-line))
        (loop :named stride-over-buffer
           :for num-read := (read-sequence buffer stream)
           :if (< num-read buffer-length)
           :do (return-from stride-over-buffer
                 (setf encoding (ces-guess-from-vector (subseq buffer 0 num-read) scheme)
                       end-of-line (eol-guess-from-vector (subseq buffer 0 num-read))))
           :else
           :do (multiple-value-bind (enc ord)
                   (ces-guess-from-vector (subseq buffer 0 num-read) scheme)
                 (setf encoding enc
                       order ord)
                 (unless end-of-line
                   (sef (eol-guess-from-vector buffer)))))
        (let ((enc-impl (dependent-name encoding))
              (eol-impl (dependent-name end-of-line)))
          (if (or (eq enc-impl :cannot-treat)
                  (eq eol-impl :cannot-treat))
              (values nil (list encoding end-of-line))
              (values
               (if eol-impl
                   (make-external-format enc-impl eol-impl)
                   (make-external-format enc-impl :lf))
               (list encoding end-of-line)))))
      (error (format nil "supplied stream is not a byte input stream."))))

(defmethod detect-external-format ((path pathname) (scheme symbol))
  "Detect external-format from `pathname`."
  (with-open-file (in path
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (detect-external-format in scheme)))
