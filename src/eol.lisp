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
