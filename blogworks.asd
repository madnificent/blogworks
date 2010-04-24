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
	       :cl-recaptcha
	       :ironclad)
  :components ((:file "packages")
	       (:file "setup" :depends-on ("packages" "model"))
	       (:file "model" :depends-on ("packages"))
	       (:file "pages" :depends-on ("packages" "model" "setup"))))
