(in-package :trivial-clipboard)

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))

(defun wayland-session-p ()
  (string= (uiop:getenvp "XDG_SESSION_TYPE") "wayland"))

(defun x-session-p ()
  (string= (uiop:getenvp "XDG_SESSION_TYPE") "x11"))

(defparameter *clipboard-commands*
  #+(or darwin macosx)
  `((:mac ,(constantly t) ("pbcopy") ("pbpaste")))
  #-(or darwin macosx)
  `((:wayland wayland-session-p ("wl-copy") ("wl-paste"))
    (:xclip
     ,(lambda () (and (executable-find "xclip") (x-session-p)))
     ("xclip" "-in" "-selection" "clipboard")
     ("xclip" "-out" "-selection" "clipboard"))
    (:xsel
     ,(lambda () (and (executable-find "xsel") (x-session-p)))
     ("xsel" "--input" "--clipboard")
     ("xsel" "--output" "--clipboard")))
  "A list, each element being of the form (clipboard-method predicate
copy-command paste-command).")

(defun clipboard-programs (fn)
  (loop :for elt :in *clipboard-commands*
        :collect (first (funcall fn elt))))

(defun get-paste-command (elt)
  (fourth elt))

(defun get-copy-command (elt)
  (third elt))

(defun find-command (fn)
  (loop :for elt :in *clipboard-commands*
        :for command := (funcall fn elt)
        :when (funcall (second elt))
        :return command))

(let ((command nil))
  (defun find-paste-command ()
    (or command
        (setf command (find-command #'get-paste-command)))))

(let ((command nil))
  (defun find-copy-command ()
    (or command
        (setf command (find-command #'get-copy-command)))))

(defun paste ()
  (let ((command (find-paste-command)))
    (if command
        (with-output-to-string (output)
          (uiop:run-program command
                            :output output))
        (error 'not-installed
               :programs (clipboard-programs #'get-paste-command)))))

(defun copy (text)
  (let ((command (find-copy-command)))
    (if command
        (with-input-from-string (input text)
          (uiop:run-program command
                            :input input))
        (error 'not-installed
               :programs (clipboard-programs #'get-copy-command)))))

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
     (copy data)
     data)
    (null
     (or
      #+os-windows
      (get-text-on-win32)
      #+(not os-windows)
      (paste)))))

(defgeneric content (&key &allow-other-keys)
  (:method (&key &allow-other-keys)
    #+os-windows (get-text-on-win32)
    #-os-windows (paste))
  (:documentation "A generic function to get the contents of the clipboard.

Returns strings by default.

:around methods and primary method re-definitions can override return
value to some structured data."))

(defgeneric (setf content) (value &key &allow-other-keys)
  (:method (value &key &allow-other-keys)
    #+os-windows (set-text-on-win32 data)
    #-os-windows (copy value)
    value)
  (:documentation "A generic function to set the contents of the clipboard to NEW-VALUE.

Default method only specializes on string, but callers can define more
methods specializing on specific data types to put into clipboard.

Example: support numbers, converting them to strings

\(defmethod (setf trivial-clipboard:content) ((value number) &key &allow-other-keys)
  (setf (trivial-clipboard:content) (princ-to-string value)))"))

(push :clipboard-content-method *features*)
