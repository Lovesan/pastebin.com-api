(in-package :cl-user)

(asdf:defsystem #:pastebin.com-api
  :depends-on (#:trivial-features #:puri #:drakma)
  :licence "Public Domain"
  :version "0.0.1"
  :serial t
  :components ((:static-file "README")
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "file-formats")
                             (:file "pastebin.com-api")))))
