(in-package :asdf-user)
(defsystem trivial-clipboard
  :version "0.0.0.0"
  :author "SANO Masatoshi"
  :description "trivial-clipboard let access system clipboard."
  :license "MIT"
  :depends-on ("uiop" #+os-windows "cffi")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 #+os-windows (:file "windows")
                 (:file "text")))))
