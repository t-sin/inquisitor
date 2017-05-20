#|
  This code is a part of inquisitor project
  and a derivate from guess (https://github.com/zqwell/guess).
|#
;;; This code is derivative of libguess-1.0 and guess-0.1.0 for common lisp.
;;; 
;;; Copyright (c) 2011 zqwell <zqwell@gmail.com>
;;; 
;;; The following is the original copyright notice.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;; 

(in-package :cl-user)
(defpackage inquisitor.encoding.guess
  (:use :cl
        :inquisitor.encoding.table)
  (:import-from :inquisitor.encoding.dfa
                :dfa-process
                :dfa-none
                :dfa-top
                :dfa-name
                :generate-order)
  (:import-from :anaphora
                :aif
                :awhen
                :it)
  (:export :list-available-scheme
           :ces-guess-from-vector))
(in-package :inquisitor.encoding.guess)


(defparameter +schemes+
  '((:jp guess-jp) ;; japanese
    (:tw guess-tw) ;; taiwanese
    (:cn guess-cn) ;; chinese
    (:kr guess-kr) ;; korean
    (:ru guess-ru) ;; russian
    (:ar guess-ar) ;; arabic
    (:tr guess-tr) ;; turkish
    (:gr guess-gr) ;; greek
    (:hw guess-hw) ;; hebrew
    (:pl guess-pl) ;; polish
    (:bl guess-bl))) ;; baltic

(defun list-available-scheme ()
  (mapcar #'car +schemes+))

(defun ces-guess-from-vector (vector scheme)
  (macrolet ((guess (fn-name) `(funcall ,fn-name vector)))
    (let ((fn-name (cadr (find scheme +schemes+ :key #'car))))
      (if fn-name
          (guess fn-name)
          (error (format nil "scheme parameter (~A): not supported." scheme))))))


(defun guess-jp (buffer)
  (block guess-body
    ;; (let* ((eucj (dfa-init +eucj-st+ +eucj-ar+ (euc-jp)))
    ;; 	      (sjis (dfa-init +sjis-st+ +sjis-ar+ (shiftjis)))
    ;; 	      (utf-8 (dfa-init +utf-8-st+ +utf-8-ar+ (utf-8)))
    ;; 	      (top  nil))
    (let ((order (generate-order :utf-8 :cp932 :euc-jp))
          (len (length buffer))
	  (c nil))
      (declare (dynamic-extent order))
      (loop for i of-type fixnum from 0 below len do
	(setf c (aref buffer (the fixnum i)))

	;; special treatment of iso-2022 escape sequence
	(when (and (= (the fixnum c) (the fixnum #x1b)) 
		   (< (the fixnum i) (the fixnum (1- len))))
	  (setf c (aref buffer (the fixnum (incf i))))
	  (when (or (= (the fixnum c) (the fixnum #x24))  ; $
		    (= (the fixnum c) (the fixnum #x28))) ; (
	    (return-from guess-body :iso-2022-jp)))

	;; special treatment of BOM
	(when (and (= (the fixnum i) (the fixnum 0))
		   (= (the fixnum c) (the fixnum #xff))
		   (< (the fixnum i) (the fixnum (1- len))))
	  (let ((c (aref buffer (the fixnum (1+ i)))))
	    (when (= (the fixnum c) #xfe)
	      (return-from guess-body :ucs-2le))))
	(when (and (= (the fixnum i) (the fixnum 0))
		   (= (the fixnum c) (the fixnum #xfe))
		   (< (the fixnum i) (the fixnum (1- len))))
	  (let ((c (aref buffer (the fixnum (1+ i)))))
	    (when (= (the fixnum c) #xff)
	      (return-from guess-body :ucs-2be))))

	(awhen (dfa-process order c)
	  (return-from guess-body it))
	(when (dfa-none order)
	  (return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))


(defun guess-tw (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :big5))
          (len (length buffer))
	  (c nil))
      (declare (dynamic-extent order))
      (loop for i of-type fixnum from 0 below len do
	(setf c (aref buffer (the fixnum i)))

	;; special treatment of iso-2022 escape sequence
	(when (and (= (the fixnum c) (the fixnum #x1b)) 
		   (< (the fixnum i) (the fixnum (1- len))))
	  (setf c (aref buffer (the fixnum (incf i))))
	  (when (or (= (the fixnum c) (the fixnum #x24))  ; $
		    (= (the fixnum c) (the fixnum #x28))) ; (
	    (return-from guess-body :iso-2022-tw)))

	;; special treatment of BOM
	(when (and (= (the fixnum i) (the fixnum 0))
		   (= (the fixnum c) (the fixnum #xff))
		   (< (the fixnum i) (the fixnum (1- len))))
	  (let ((c (aref buffer (the fixnum (1+ i)))))
	    (when (= (the fixnum c) #xfe)
	      (return-from guess-body :ucs-2le))))
	(when (and (= (the fixnum i) (the fixnum 0))
		   (= (the fixnum c) (the fixnum #xfe))
		   (< (the fixnum i) (the fixnum (1- len))))
	  (let ((c (aref buffer (the fixnum (1+ i)))))
	    (when (= (the fixnum c) #xff)
	      (return-from guess-body :ucs-2be))))

	(awhen (dfa-process order c)
	  (return-from guess-body it))
	(when (dfa-none order)
	  (return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))


(defun guess-cn (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :gb2312 :gb18030))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of iso-2022 escape sequence
	      (when (and (= (the fixnum c) (the fixnum #x1b)) 
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i))))
		      (c2 (aref buffer (the fixnum (+ i 2)))))
		  (when (and (= (the fixnum c) (the fixnum #x24))        ; $
			     (or (= (the fixnum c2) (the fixnum #x29))   ; )
				 (= (the fixnum c2) (the fixnum #x2B)))) ; +
		    (return-from guess-body :iso-2022-cn))))

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))


(defun guess-kr (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :euc-kr :johab))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of iso-2022 escape sequence
	      (when (and (= (the fixnum c) (the fixnum #x1b)) 
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i))))
		      (c2 (aref buffer (the fixnum (+ i 2)))))
		  (when (and (= (the fixnum c) (the fixnum #x24))   ; $
			     (= (the fixnum c2) (the fixnum #x29))) ; )
		    (return-from guess-body :iso-2022-kr))))

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))

(defun guess-ar (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :iso-8859-6 :cp1256))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))

(defun guess-gr (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :iso-8859-7 :cp1253))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))

(defun guess-ru (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :cp1251 :koi8-u :koi8-r :cp866
			     :iso-8859-2 :iso-8859-5))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))

(defun guess-hw (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :iso-8859-8 :cp1255))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))

(defun guess-pl (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :cp1250 :iso-8859-2))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))


(defun guess-tr (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :iso-8859-9 :cp1254))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))


(defun guess-bl (buffer)
  (block guess-body
    (let ((order (generate-order :utf-8 :iso-8859-13 :cp1257))
          (len (length buffer)))
      (declare (dynamic-extent order))
      (loop for c of-type fixnum across buffer
	    for i of-type fixnum from 0 do

	      ;; special treatment of BOM
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xff))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xfe)
		    (return-from guess-body :ucs-2le))))
	      (when (and (= (the fixnum i) (the fixnum 0))
			 (= (the fixnum c) (the fixnum #xfe))
			 (< (the fixnum i) (the fixnum (1- len))))
		(let ((c (aref buffer (the fixnum (1+ i)))))
		  (when (= (the fixnum c) #xff)
		    (return-from guess-body :ucs-2be))))
	      
	      (awhen (dfa-process order c)
		(return-from guess-body it))
	      (when (dfa-none order)
		(return-from guess-body  nil)))

      (aif (dfa-top order)
	  (dfa-name it)
	  nil))))
