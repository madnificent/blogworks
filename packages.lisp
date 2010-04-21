(defpackage :blogworks.packages
  (:use :common-lisp))
(in-package :blogworks.packages)

(defpackage :blogworks.model
  (:use :cl :bknr.datastore :bknr.indices)
  (:export :user :nickname :password :email-address :find-user-by-nickname :find-user-by-name-and-password :latest-users
	   :blog :owner :blogs-of-user :title :name :description :latest-blogs :find-blog-by-name
	   :post :posts-in-blog :title :content :latest-posts :find-post-by-blog-and-title
	   :comment :owner :parent :title :content :comments-of :comments-on
	   :delete-object :remove-object))

(defpackage :blogworks.site
  (:use :common-lisp
	:cw.site
 	:blogworks.model)
  (:shadowing-import-from :blogworks.model :title :name :content)
  (:export :welcome :new-user :create-user :login :personal-user-page :public-user-page :edit-user-page :update-user-page :logout :blog-page :delete-blog-page :post-page :new-blog :create-blog :new-post :create-post :create-comment :edit-post :update-post)
  (:export :nickname :password :email :logged-in-user :redirect-to-welcome :user :blog :post :post-title :unknown-user-login :delete-blog))

(defpackage :blogworks.routing
  (:use :common-lisp
	:blogworks.site
	:blogworks.model
	:claymore.routing))

(defpackage :blogworks.setup
  (:use :common-lisp
	:cw.setup.helpers)
  (:export :*wysiwyg-path* :*css-path* :*google-analytics-key* :*recaptcha-p*))
