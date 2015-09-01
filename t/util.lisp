(in-package :cl-user)
(defpackage inquisitor-test.util
  (:use :cl))
(in-package :inquisitor-test.util)


(defun get-test-data (pathname)
  (merge-pathnames pathname *load-truename*))
