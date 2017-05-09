(in-package :asdf-user)
(defsystem trivial-clipboard
  :version "0.0.0.0"
  :author "SANO Masatoshi"
  :description "trivial-clipboard let access system clipboard."
  :license "MIT"
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "text")))))
