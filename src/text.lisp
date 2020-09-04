(in-package :trivial-clipboard)

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))

(defvar *clipboard-in-command*
  #+(or darwin macosx)
  "pbcopy"
  #+(and :unix (:not :darwin))
  (or (executable-find "wl-copy")
      (executable-find "xclip")
      (executable-find "xsel")
      ""))

(defvar *clipboard-out-command*
  #+(or darwin macosx)
  "pbpaste"
  #+(and :unix (:not :darwin))
  (or (executable-find "wl-paste")
      *clipboard-in-command*))

(defvar *clipboard-in-args*
  (progn
    '()
    #+ (and :unix (:not :darwin))
    (or (and (string= (pathname-name *clipboard-in-command*) "wl-copy")
             '())
        (and (string= (pathname-name *clipboard-in-command*) "xclip")
             '("-in" "-selection" "clipboard"))
        (and (string= (pathname-name *clipboard-in-command*) "xsel")
             '("--input" "--clipboard")))))

(defvar *clipboard-out-args*
  (progn
    '()
    #+ (and :unix (:not :darwin))
    (or (and (string= (pathname-name *clipboard-in-command*) "wl-paste")
             '())
        (and (string= (pathname-name *clipboard-in-command*) "xclip")
             '("-out" "-selection" "clipboard"))
        (and (string= (pathname-name *clipboard-in-command*) "xsel")
             '("--output" "--clipboard")))))

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
