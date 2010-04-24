(in-package :blogworks.model)

(defun hash-password (password)
  "Creates a hashed version of the given password string"
  (declare (type string password))
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array password))))

(define-persistent-class user ()
  ((nick :read 
	 :reader nickname
	 :index-type string-unique-index
	 :index-reader find-user-by-nickname
	 :index-values all-users)
   (pass :update
	 :reader password)
   (email :update
	  :accessor email-address
	  :index-type string-unique-index
	  :index-reader user-by-email
	  :index-values all-email-addresses))
  (:documentation "Standard user of the blogging system"))
(defmethod (setf password) (password (user user))
  (with-slots (pass) user
    (setf pass (hash-password password))))
(defmethod initialize-instance :after ((user user) &key pass)
  (setf (password user) pass))

(define-persistent-class blog ()
  ((owner :read
	  :reader owner
	  :index-type hash-index
	  :index-reader blogs-of-user
 	  :index-values all-blogs)
   (title :update
	  :accessor title)
   (name :read
	 :reader name
	 :index-type string-unique-index
	 :index-reader find-blog-by-name)
   (description :update
		:accessor description))
  (:documentation "Represents one of the blogs of a user"))

(define-persistent-class has-comments ()
  ()
  (:documentation "A class in which the objects may have comments"))

(define-persistent-class post (has-comments)
  ((blog :read
	 :reader blog
	 :index-type hash-index
	 :index-reader posts-in-blog
	 :index-values all-posts)
   (title :update
	  :accessor title)
   (content :update
	    :accessor content))
  (:documentation "Represents a post in the blog"))

(define-persistent-class comment (has-comments)
  ((owner :read
	  :reader owner
	  :index-type hash-index
	  :index-reader comments-of)
   (parent :read
	   :reader parent
	   :index-type hash-index
	   :index-reader comments-on
	   :index-values all-comments)
   (title :update
	  :accessor title)
   (content :update
	    :accessor content))
  (:documentation "Represents a comment on a post"))



;;;;;;;;;;;;;;;;;;;
;; helper functions
(defun create-new-user (nick pass email)
  (make-instance 'user
		 :nick nick
		 :pass pass
		 :email email))

(defun find-user-by-name-and-password (nick password)
  (let ((user (find-user-by-nickname nick)))
    (when (and user (equal (hash-password password) (user-pass user)))
      user)))

(defun find-post-by-blog-and-title (blog title)
  (loop for post in (posts-in-blog blog)
     when (string= (post-title post) title)
     return post))

(defun latest-users (amount)
  (loop for x from 0 below amount
     for user in (all-users)
     collect user)) ;; this might not guarantee to be the last users (I don't know :()

(defun create-new-blog (owner title description name)
  (make-instance 'blog :owner owner :title title :description description :name name))

(defun latest-blogs (amount)
  (loop for x from 0 below amount
     for blog in (all-blogs)
     collect blog))

(defun create-new-post (blog title content)
  (make-instance 'post :blog blog :title title :content content))

(defun get-posts-for-blog (blog)
  (posts-in-blog blog))

(defun latest-posts (amount)
  (loop for x from 0 below amount
     for post in (all-posts)
     collect post))

(defun create-new-comment (post title content owner)
  (make-instance 'comment :parent post :title title :content content :owner owner))

(defun get-comments-for-post (post)
  (comments-on post))


(defgeneric remove-object (object)
  (:documentation "Removes an object from the store"))
(defmethod remove-object (object)
  (delete-object object))
(defmethod remove-object :before ((user user))
  (loop for blog in (blogs-of-user user)
      do (remove-object blog))
  (loop for comment in (comments-of user)
     do (remove-object comment)))
(defmethod remove-object :before ((blog blog))
  (loop for post in (posts-in-blog blog)
     do (remove-object post)))
(defmethod remove-object :before ((commentable has-comments))
  (loop for comment in (comments-on commentable)
     do (remove-object comment)))

