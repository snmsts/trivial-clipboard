(cl:in-package :cl-user)

(defpackage trivial-clipboard
  (:use :cl #+os-windows :cffi)
  (:export :text))
