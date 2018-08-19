;; -*- encoding: utf-8 -*-

(cl:in-package :cl-user)

(defpackage trivial-clipboard.test
  (:use :cl :fiveam :trivial-clipboard))

(in-package :trivial-clipboard.test)

(def-suite :trivial-clipboard)
(in-suite :trivial-clipboard)

(defparameter *standard-chars* " !\"#$%&'()*+,-./0123456789
:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
[\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defparameter *multibyte-chars* "日本語
汉语
اللغة العربية
русский язык")

(defparameter *emoji-chars* "😀😁😂
💪♐🌵
🇦🆗⬇")

(defparameter *mixed-newlines*
  (format nil "CR~ACR+LF~A~ALF~AOK?"
          (code-char #x0d)
          (code-char #x0d)
          (code-char #x0a)
          (code-char #x0a)))

(defun test-set-and-get (string)
  (string= string
           (progn (text string)
                  (text))))

(test basic-set
  (is (test-set-and-get *standard-chars*))
  (is (test-set-and-get *multibyte-chars*))
  (is (test-set-and-get *emoji-chars*))
  (signals type-error (text 1)))

(test extra-set
  (is (test-set-and-get *mixed-newlines*))
  ;; All ASCII characters. Maybe unnecessary to handle all control chars?
  (dotimes (i 128)
    (let ((char (code-char i)))
      (is (test-set-and-get (format nil "~A equals ~A or ~A" char char char))))))
