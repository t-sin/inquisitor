(in-package :cl-user)
(defpackage inquisitor.eol
  (:use :cl)
  (:export :eol-available-p
           :eol-guess-from-vector))
(in-package :inquisitor.eol)


(defun eol-available-p ()
  ;; #+allegro
  ;; #+lispworks
  #+clisp t
  #+ecl t
  #+sbcl nil
  #+ccl t
  #+abcl t)


(defun eol-guess-from-vector (buffer)
  (let ((len (length buffer)))
    (loop for i of-type fixnum from 0 below len
       finally (return nil)
       do (if (eq (aref buffer (the fixnum i))
                  (the fixnum (char-code #\Return)))
              (if (and (< (1+ (the fixnum i)) len)
                       (eq (aref buffer (the fixnum (1+ i)))
                           (the fixnum (char-code #\Newline))))
                  (return :crlf)
                  (return :cr)))
         (if (eq (aref buffer (the fixnum i))
                 (the fixnum (char-code #\Newline)))
             (return :lf)))))
