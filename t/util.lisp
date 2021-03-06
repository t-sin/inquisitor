(in-package :cl-user)
(defpackage inquisitor.util-test
  (:use :cl
        :inquisitor.util
        :prove)
  (:import-from :inquisitor.util
                :with-byte-array))
(in-package :inquisitor.util-test)

;; NOTE: To run this test file, execute `(asdf:test-system :inquisitor)' in your Lisp.

(plan 3)


(subtest "with-byte-array"
  (subtest "expansion"
    (is-expand (with-byte-array (vec 10)
                 (print vec)
                 (svref vec 0))
               (let ((vec (make-array 10 :element-type '(unsigned-byte 8))))
                 (print vec)
                 (svref vec 0))))

  (with-byte-array (vec 10)
    (is (length vec) 10)
    (ok (loop for e across vec
           always (eq e 0)))))

(subtest "byte-array-p"
  (subtest "return nil"
    (ok (not (byte-array-p 42)))
    (ok (not (byte-array-p nil)))
    (ok (not (byte-array-p "string"))))

  (subtest "return t"
    (ok (byte-array-p (make-array 0 :element-type '(unsigned-byte 8))))
    (ok (byte-array-p (make-array 1 :element-type '(unsigned-byte 8))))))

(subtest "byte-input-stream-p"
  (subtest "return nil"
    (ok (not (byte-input-stream-p "string")))
    (with-output-to-string (out)
      (ok (not (byte-input-stream-p out))))
    (with-input-from-string (in "string")
      (ok (not (byte-input-stream-p in))))
    (with-open-file (in (asdf:system-relative-pathname :inquisitor "t/data/ascii/ascii.txt")
                        :direction :input
                        :element-type 'character)
      (ok (not (byte-input-stream-p in)))))

  (subtest "return t"
    (with-open-file (in (asdf:system-relative-pathname :inquisitor "t/data/ascii/ascii.txt")
                        :direction :input
                        :element-type '(unsigned-byte 8))
      (ok (byte-input-stream-p in)))))


(finalize)
