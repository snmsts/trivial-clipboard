(in-package :trivial-clipboard)

(defvar *clipboard-in-command*
  #+(or darwin macosx)
  "pbcopy"
  #+(and :unix (:not :darwin))
  "xclip")

(defvar *clipboard-out-command*
  #+(or darwin macosx)
  "pbpaste"
  #+(and :unix (:not :darwin))
  "xclip")

(defvar *clipboard-in-args*
  (progn
    '()
    #+ (and :unix (:not :darwin))
    '("-in" "-selection" "clipboard")))

(defvar *clipboard-out-args*
  (progn
    '()
    #+ (and :unix (:not :darwin))
    '("-out" "-selection" "clipboard")))

(defun text (&optional data)
  "If DATA is STRING, it is set to the clipboard. An ERROR is
signalled if the copy failed.

If DATA is NIL, TEXT returns the STRING from the clipboard. If the
copy failed, it returns NIL instead."
  (etypecase data
    (string
     #+os-windows
     (set-text-on-win32 data)
     #+(not os-windows)
     (with-input-from-string (input data)
       (uiop:run-program (cons *clipboard-in-command* *clipboard-in-args*)
                         :input input))
     data)
    (null
     (or
      #+os-windows
      (get-text-on-win32)
      #+(not os-windows)
      (with-output-to-string (output)
         (uiop:run-program (cons *clipboard-out-command* *clipboard-out-args*)
                           :output output))))))
