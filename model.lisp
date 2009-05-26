(in-package :blogworks.model)

(defmacro with-db (&body body)
  `(postmodern:with-connection database-migrations:*db-connection-parameters*
     ,@body))

(defclass blog-user ()
  ((user-id :primary-key t :reader id)
   (password :accessor password)
   (nick :accessor nick)
   (email :accessor email)
   (blogs :referenced-from blog :on owner :accessor blogs))
  (:metaclass standard-db-access-class)
  (:documentation "Standard user of the blogging system"))

(defclass blog ()
  ((blog-id :primary-key t :reader id)
   (owner :column owner :references blog-user :accessor owner)
   (title :accessor title)
   (description :accessor description)
   (posts :referenced-from post :on blog :accessor posts))
  (:metaclass standard-db-access-class)
  (:documentation "Represents one of the blogs of a user"))

(defclass post ()
  ((post-id :primary-key t :reader id)
   (blog :column blog :references blog :reader blog)
   (title :accessor title)
   (content :accessor content)
   (comments :referenced-from comment :on post :accessor comments))
  (:metaclass standard-db-access-class)
  (:documentation "Represents a post in the blog"))

(defclass comment ()
  ((comment-id :primary-key t :reader id)
   (owner :references blog-user :reader owner)
   (post :column post :references post :reader post)
   (title :accessor title)
   (content :accessor content))
  (:metaclass standard-db-access-class)
  (:documentation "Represents a comment on a post"))

(defun create-new-user (nick pass email)
  (with-db (insert-object (make-object 'blog-user 'nick nick 'password pass 'email email))))

(defun find-user-by-name-and-password (nick password)
  (with-db (first (select-using-object 
		   (make-object 'blog-user :nick nick :password password)))))

(defun find-user-by-id (id)
  (let ((id (if (stringp id) (parse-integer id) id)))
    (with-db (rofl:find-object 'blog-user id))))

(defun get-latest-users (amount)
  (with-db (select-only-n-objects amount 'blog-user)))

(defun create-new-blog (owner title description)
  (with-db (insert-object (make-object 'blog 'owner owner 'title title 'description description))))

(defun find-blog-by-id (id)
  (let ((id (if (stringp id) (parse-integer id) id)))
    (with-db (find-object 'blog id))))

(defun get-latest-blogs (amount)
  (with-db (select-only-n-objects amount 'blog)))

(defun create-new-post (blog title content)
  (with-db (insert-object (make-object 'post 'blog blog 'title title 'content content))))

(defun get-posts-for-blog (blog)
  (with-db (posts blog)))

(defun get-latest-posts (amount)
  (with-db (select-only-n-objects amount 'post)))

(defun find-post-by-id (id)
  (let ((id (if (stringp id) (parse-integer id) id)))
    (with-db (find-object 'post id))))

(defun create-new-comment (post title content owner)
  (with-db (insert-object (make-object 'comment 'owner owner 'post post 'title title 'content content))))

(defun get-comments-for-post (post)
  (with-db (comments post)))

(defun update-modified-post (post)
  (with-db (update-object post)))

(defgeneric remove-object (object)
  (:documentation "Removes the given object from the database"))
(defmethod remove-object (object)
  (with-db (delete-object object)))
