(defpackage :blogworks.packages
  (:use :common-lisp))

(in-package :blogworks.packages)

(defpackage :blogworks.model
  (:use :common-lisp
	:rofl)
  ;; bare model
  (:export :blog-user :id :user-id :password :nick :email :blogs
	   :blog :blog-id :owner :title :description :posts
	   :post :post-id :blog :title :content :comments
	   :comment :comment-id :post :title :content)
  ;; extras
  (:export :with-db)
  (:export :find-user-by-name-and-password :create-new-user
	   :get-latest-users :find-user-by-id)
  (:export :create-new-blog :find-blog-by-id
	   :get-latest-blogs)
  (:export :create-new-post :get-latest-posts :get-posts-for-blog
	   :find-post-by-id :update-modified-post)
  (:export :create-new-comment :get-comments-for-post))

(defpackage :blogworks.site
  (:use :common-lisp
	:blogworks.model
	:claymore
	:claymore.html)
  (:shadowing-import-from :blogworks.model :title)
  (:export :welcome :new-user :create-user :login :personal-user-page :public-user-page :logout :blog-page :post-page :new-blog :create-blog :new-post :create-post :create-comment :edit-post :update-post)
  (:export :nickname :password :email :logged-in-user :redirect-to-welcome :user :blog :post))

(defpackage :blogworks.routing
  (:use :common-lisp
	:blogworks.model
	:blogworks.site
	:claymore.routing))

(defpackage :blogworks.setup
  (:use :common-lisp
	:hunchentoot)
  (:export :*wysiwyg-path* :*css-path* :*google-analytics-key*))
