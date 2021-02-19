(in-package :trivial-clipboard)

(define-condition trivial-clipboard-error (simple-error) ())

(define-condition not-installed (trivial-clipboard-error)
  ((programs
    :initarg :programs
    :reader not-installed-programs))
  (:report (lambda (c s)
             (format s "None of the commands are installed: ~S"
                     (not-installed-programs c)))))
