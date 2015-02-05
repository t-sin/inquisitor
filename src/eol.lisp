(in-package :cl-user)
(defpackage inquisitor.eol
  (:use :cl
        :cl-annot))
(in-package :inquisitor.eol)

(enable-annot-syntax)


@export
(defun eol-available-p ()
  ;; #+allegro
  ;; #+lispworks
  #+clisp t
  #+ecl t
  #+sbcl nil
  #+ccl t
  #+abcl t)


@export
(defun lf-keyword ()
  ;; #+lispworks :lf
  #+clisp :unix
  ;; #+ecl :lf
  ;; #+sbcl nil ; sbcl cannot treat line-breaks
  #+ccl :unix
  ;; #+abcl :lf
  #-(and clisp ccl) :lf)

@export
(defun cr-keyword ()
  ;; #+lispworks :cr
  #+clisp :mac
  ;; #+ecl :cr
  ;; #+sbcl nil ; sbcl cannot treat line-breaks
  #+ccl :macos
  ;; #+abcl :cr
  #-(and clisp ccl) :cr)

@export
(defun crlf-keyword ()
  ;; #+lispworks :crlf
  #+clisp :dos
  ;; #+ecl :crlf
  ;; #+sbcl nil ; sbcl cannot treat line-breaks
  #+ccl :dos
  ;; #+abcl :crlf
  #-(and clisp ccl) :crlf)


@export
(defun eol-guess-from-vector (buffer &aux (len (length buffer)))
  (loop for i of-type fixnum from 0 below len do
       (if (eq (aref buffer (the fixnum i))
               (the fixnum (char-code #\Return)))
           (if (and (< (1+ (the fixnum i)) len)
                    (eq (aref buffer (the fixnum (1+ i)))
                        (the fixnum (char-code #\Newline))))
               (return (crlf-keyword))
               (return (cr-keyword))))
       (if (eq (aref buffer (the fixnum i))
               (the fixnum (char-code #\Newline)))
           (return (lf-keyword)))))
