(defpackage :blogworks.sysdef
  (:use :common-lisp
	:asdf))

(in-package :blogworks.sysdef)

(defsystem :blogworks
  :name "Blogworks -- a blog based on clayworks"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "A blogging system based on the clayworks web development stack"
  :depends-on (:clayworks
	       :cl-recaptcha)
  :components ((:file "packages")
	       (:file "migrations" :depends-on ("packages" "setup"))
	       (:file "model" :depends-on ("packages" "setup"))
	       (:file "setup" :depends-on ("packages"))
	       (:file "pages" :depends-on ("packages" "model"))))
