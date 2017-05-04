(in-package :asdf-user)
(defsystem sn.clipboard
  :version "0.0.0.0"
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "text")))))
