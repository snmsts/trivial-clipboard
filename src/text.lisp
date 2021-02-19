(in-package :trivial-clipboard)

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))

(let ((table (make-hash-table :test 'equal)))
  (defun executable-find-with-cache (command)
    (or (gethash command table)
        (setf (gethash command table)
              (executable-find command)))))

(defvar *clipboard-commands* '())

(defun register-clipboard (name copy-command paste-command)
  (let ((elt (assoc name *clipboard-commands*))
        (value (list (uiop:ensure-list copy-command)
                     (uiop:ensure-list paste-command))))
    (if elt
        (setf (cdr elt)
              value)
        (setf *clipboard-commands*
              (append *clipboard-commands*
                      (list (cons name value)))))))

#+(or darwin macosx)
(register-clipboard :mac
                    "pbcopy"
                    "pbpaste")

#-(or darwin macosx)
(progn
  (register-clipboard :wayland
                      "wl-copy"
                      "wl-paste")

  (register-clipboard :xclip
                      '("xclip" "-in" "-selection" "clipboard")
                      '("xclip" "-out" "-selection" "clipboard"))

  (register-clipboard :xsel
                      '("xsel" "--input" "--clipboard")
                      '("xsel" "--output" "--clipboard")))

(defun get-paste-command (elt)
  (third elt))

(defun get-copy-command (elt)
  (second elt))

(defun find-command (fn)
  (loop :for elt :in *clipboard-commands*
        :for command := (funcall fn elt)
        :when (executable-find-with-cache (first command))
        :return command))

(defun find-paste-command ()
  (find-command #'get-paste-command))

(defun find-copy-command ()
  (find-command #'get-copy-command))

(defun paste ()
  (let ((command (find-paste-command)))
    (when command
      (with-output-to-string (output)
        (uiop:run-program command
                          :output output)))))

(defun copy (text)
  (let ((command (find-copy-command)))
    (when command
      (with-input-from-string (input text)
        (uiop:run-program command
                          :input input)))))

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
