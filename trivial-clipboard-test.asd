(in-package :asdf-user)
(defsystem trivial-clipboard-test
  :author "SANO Masatoshi"
  :depends-on ("trivial-clipboard" "fiveam")
  :components ((:file "test"))
  :perform (test-op (o s)
                    (symbol-call :fiveam :run! :trivial-clipboard)))
