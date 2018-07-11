(defpackage trivial-clipboard
  (:use :cl)
  (:export :text))

(in-package :trivial-clipboard)

(defun text (&optional data)
  (cond
    ((stringp data)
     (or
      (or
       #+(or darwin macosx)
       (ignore-errors
         (with-input-from-string (input data)
           (uiop:run-program "pbcopy"
                             :input input))
         t)
       (ignore-errors
         (with-input-from-string (input data)
           (uiop:run-program "xclip -i -selection clipboard"
                             :input input))
         t)
       (ignore-errors
         (with-input-from-string (input data)
           (uiop:run-program "xsel -bi" :input input))
         t))
      (error "copy failure"))
     data)
    ((null data)
     (or
      #+(or darwin macosx)
      (ignore-errors
        (with-output-to-string (output)
          (uiop:run-program "pbpaste"
                            :output output)))
      (ignore-errors
        (with-output-to-string (output)
          (uiop:run-program "xclip -o -selection clipboard"
                            :output output)))
      (ignore-errors
        (with-output-to-string (output)
          (uiop:run-program "xsel -bo" :output output)))))
    (t (error "~S is not acceptable." data))))
