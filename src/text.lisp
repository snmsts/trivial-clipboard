(in-package :trivial-clipboard)

(defun text (&optional data)
  "If DATA is STRING, it is set to the clipboard. An ERROR is
signalled if the copy failed.

If DATA is NIL, TEXT returns the STRING from the clipboard. If the
copy failed, it returns NIL instead."
  (etypecase data
    (string
     (or
      #+(or darwin macosx)
      (ignore-errors
       (with-input-from-string (input data)
         (uiop:run-program "pbcopy"
                           :input input))
       t)
      #+os-windows
      (ignore-errors (set-text-on-win32 data))
      (ignore-errors
       (with-input-from-string (input data)
         (uiop:run-program "xclip -i -selection clipboard"
                           :input input))
       t)
      (ignore-errors
       (with-input-from-string (input data)
         (uiop:run-program "xsel -bi" :input input))
       t)
      (error "copy failure"))
     data)
    (null
     (or
      #+(or darwin macosx)
      (ignore-errors
       (with-output-to-string (output)
         (uiop:run-program "pbpaste"
                           :output output)))
      #+os-windows
      (ignore-errors (get-text-on-win32))
      (ignore-errors
       (with-output-to-string (output)
         (uiop:run-program "xclip -o -selection clipboard"
                           :output output)))
      (ignore-errors
       (with-output-to-string (output)
         (uiop:run-program "xsel -bo" :output output)))))))
