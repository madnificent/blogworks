;;;; TODO: check wether or not the logged-in-user is the correct user for blog creation etc

(in-package :blogworks.routing)

(defhandles as-failsafe-for (failsafe &rest other-cases)
  "Allows you to create a failsafe option for simple cases. Only expands into cases that start by stealing a part of the url (like \"new\" or \"login\")"
  `(,@(map 'list (lambda (subroute) 
		   (if (and (listp subroute) (stringp (first subroute)))
		       `(,(first subroute) (handles ,failsafe as-failsafe-for ,@(rest subroute)))
		       subroute))
	   other-cases)
      ,failsafe))

(def-identification-handler blog-user (lambda (user) (write-to-string (blogworks.model:id user))) 'blogworks.model:find-user-by-id)
(def-identification-handler blog (lambda (blog) (write-to-string (blogworks.model:id blog))) 'blogworks.model:find-blog-by-id)
(def-identification-handler post (lambda (post) (write-to-string (blogworks.model:id post))) 'blogworks.model:find-post-by-id)

(defwhen logged-in-user?
    "Evaluates when the current request was a post-request"
  ((&rest items) 
   (when (claymore:param* 'logged-in-user)
     items))
  ((&rest items)
   (when (claymore:param* 'logged-in-user)
     items)))
(defwhen user-not-logged-in
    "Ensures the user is not logged in"
  ((&rest items) 
   (unless (claymore:param* 'logged-in-user)
     items))
  ((&rest items)
   (unless (claymore:param* 'logged-in-user)
     items)))


(add-static-routing-dispatcher "/wysiwyg/" blogworks.setup:*WYSIWYG-PATH*)
(add-static-routing-dispatcher "/css/" blogworks.setup:*CSS-PATH*)

(set-routing-table '(welcome
		     (handles redirect-to-welcome as-failsafe-for
		      ("new" (when user-not-logged-in 
			       (when get-request new-user)
			       (when post-request create-user)))
		      ("login" (when post-request login))
		      ("logout" (when post-request logout))
		      ("users"
		       (when logged-in-user? personal-user-page)
		       (".*" (handler identifies blog-user as user)
			     public-user-page))
		      ("blogs"
		       (when logged-in-user? 
			 ("new" (when get-request new-blog)
				(when post-request create-blog)))
		       (".*" (handler identifies blog as blog)
			     blog-page
			     ("posts"
			      (when logged-in-user?
				(when get-request new-post)
				(when post-request create-post))
			      (".*" (handler identifies post as post)
				    post-page
				    ("comments" (when logged-in-user? (when post-request create-comment)))))))
		      ("posts"
		       (".*" (handler identifies post as post)
			     ("edit" (when logged-in-user? 
				       (when get-request edit-post)
				       (when post-request update-post)))
			     post-page)))))

(in-package :blogworks.site)

(defparameter *title-prefix* "BlogWorks")
(defparameter *title-separator* " : ")
(defparameter *link-separator* " | ")

;;;;;;;;;;;;;;;;;;;
;; general
(defparameter *scripts* `(:wysiwyg (,(script :language "JavaScript" :type "text/javascript" :src "/wysiwyg/scripts/wysiwyg.js" "")
				    ,(script :language "JavaScript"
					     "var settings = new WYSIWYG.Settings();"
					     "WYSIWYG.attach('all', settings);"
					     "settings.ImagesDir = \"/wysiwyg/images/\";"
					     "settings.PopupsDir = \"/wysiwyg/popups/\";"
					     "settings.CSSFile = \"/wysiwyg/styles/wysiwyg.css\";"))))

(defun standard-surrounded-page (subtitle &key operations content scripts)
  (html (head
	 (claymore.html:title *title-prefix* *title-separator* subtitle)
	 (link :rel "stylesheet" :type "text/css" :href "/css/base.css")
	 (loop for script in scripts collect
	      (getf *scripts* script)))
	(body (div :class "userOperations" (h1 subtitle) (div (user-login-operations)))
	      (div :class "content" content)
	      (div :class "operations" (link-to-page "Home" 'welcome) (loop for o in operations collect (list *link-separator* o))))))

(defpage redirect-to-welcome ()
  (redirect-to-page 'welcome))

(defpage welcome (logged-in-user)
  (standard-surrounded-page 
   "Welcome"
   :content (list (p "Hello user, welcome to the blogworks blogging platform.  Feel free to login, create an account or sniff around.")
		  (new-users)
		  (new-blogs)
		  (new-posts))
   :operations (unless logged-in-user (list (link-to-page "new user" 'new-user)))))

;;;;;;;;;;;;;;;;;;;
;; login
(defpage user-login-operations (logged-in-user)
  (if logged-in-user
      (list (logout-button) " | " (link-to-page "my page" 'personal-user-page))
      (inline-login-form)))

(defun inline-login-form ()
  (span :class "loginStuff"
	(form :method "post" :action (claymore.routing:handler-url 'login)
	      "nick" (claymore.html.full:input :type "text" :name "NICK")
	      "pass" (claymore.html.full:input :type "password" :name "PASSWORD")
	      (submit-button :value "login"))))

(defun logout-button ()
  (bttn-to 'logout "logout"))

(defpage login (nick password)
  (let ((user (find-user-by-name-and-password nick password)))
    (when user
      (setf (hunchentoot:session-value 'logged-in-user) user)
      (hunchentoot:redirect (hunchentoot:referer)))))

(defpage logout ()
  (declare (special *session*))
  (hunchentoot:remove-session hunchentoot:*session*)
  (hunchentoot:redirect (hunchentoot:referer)))

;;;;;;;;;;;;;;;;;;;
;; users
(defun new-users (&optional (amount 10))
  (div :class "stat"
       (h1 "new users")
       (ul (loop for user in (get-latest-users amount) collect
		(li (link-to-page (nick user) 'public-user-page `(user ,user)) " :: " (email user))))))

(defpage new-user ()
  (standard-surrounded-page 
   "New User"
   :content (list (p "please fill out the data below to create a new user")
		  (form :method "post" :action (claymore.routing:handler-url 'create-user)
			(text-field "NICKNAME" T) (br)
			(strong "PASSWORD") (claymore.html.full:input :type "password" :name "PASSWORD") (br)
			(text-field "EMAIL" T) (br)
			(submit-button :value "create")))))

(defpage create-user (nickname password email)
  (create-new-user nickname password email)
  (redirect-to-page 'welcome))

(defpage personal-user-page (logged-in-user)
  (unless logged-in-user
    (redirect-to-welcome))
  (public-user-page :user logged-in-user :logged-in-user logged-in-user))

(defpage public-user-page (user logged-in-user)
  (standard-surrounded-page
   (list "user page for " (nick user))
   :content (list (h2 "Blogs of " (nick user)) (blogs-for-user user))
   :operations (list (if (and logged-in-user (eql (id user) (id logged-in-user)))
			 (list (link-to-page "New blog" 'new-blog))))))
  
;;;;;;;;;;;;;;;;;;;
;; blogs
(defun blogs-for-user (user)
  (ul :class "blogs"
      (loop for blog in (with-db (blogs user)) collect
	   (li (link-to-page (title blog) 'blog-page `(blog ,blog))))))

(defun new-blogs (&optional (amount 10))
  (div :class "stat"
       (h1 "latest blogs")
       (ul (loop for blog in (get-latest-blogs amount) collect
		(li (link-to-page (title blog) 'blog-page `(blog ,blog)))))))

(defpage new-blog (logged-in-user)
  (standard-surrounded-page 
   "New Blog"
   :content (list (h1 "Create a new blog for " (nick logged-in-user))
		  (p "Please enter the required data about the blog below")
		  (form :method "post" :action (claymore.routing:handler-url 'create-blog)
			(table (tr
				(td "title") (td (text-field "TITLE" nil)))
			       (tr
				(td "description") (td (textarea :name "DESCRIPTION" :id "textarea1" :rows "20" :cols "60" "")))
			       (tr
				(td (submit-button :value "create"))))))
   :scripts '(:wysiwyg)))

(defpage create-blog (logged-in-user title description)
  (create-new-blog logged-in-user title description)
  (redirect-to-welcome))
		 
(defpage blog-page (blog logged-in-user)
  (let ((owner (with-db (owner blog))))
    (standard-surrounded-page
     (list "Blog" *title-separator* (title blog))   
     :content (list (p (description blog))
		    (if (get-posts-for-blog blog)
			(list (h2 "Posts in " (title blog))
			      (posts-for-blog blog))
			(h2 "No posts for " (title blog))))
     :operations (list (if (and logged-in-user (eql (id (owner blog)) (id logged-in-user)))
			   (link-to-page "new post" 'new-post `(blog ,blog))
			   (link-to-page (nick owner) 'public-user-page `(user ,owner)))))))

;;;;;;;;;;;;;;;;;;;
;; posts
(defun new-posts ()
  (div :class "stat"
       (h1 "Latest posts")
       (ul (loop for post in (get-latest-posts 10) collect
		(li (link-to-page (title post) 'post-page `(post ,post blog ,(with-db (blog post)))))))))

(defun posts-for-blog (blog)
  (ol :class "posts"
      (loop for post in (get-posts-for-blog blog) collect
	   (li (link-to-page (title post) 'post-page `(post ,post))))))

(defpage new-post (blog)
  (standard-surrounded-page
   "New Post"
   :content (list (h1 "Create a new post for " (title blog))
		  (p "Please enter your post below")
		  (form :method "post" :action (claymore.routing:handler-url 'create-post 'blog blog)
			(table (tr
				(td "title") (td (text-field "TITLE" nil)))
			       (tr
				(td "content") (td (textarea :name "CONTENT" :id "textarea1" :rows "20" :cols "60" "")))
			       (tr
				(td (submit-button :value "create"))))))
   :scripts '(:wysiwyg)
   :operations `(,(link-to-page "back" 'blog-page `(blog ,blog)))))
  
(defpage create-post (blog title content)
  (create-new-post blog title content)
  (redirect-to-welcome))

(defpage edit-post (post logged-in-user)
  (let* ((blog (with-db (blog post)))
	 (owner (with-db (owner blog))))
    (unless (eql (id owner) (id logged-in-user))
      (redirect-to-page 'post-page :page-options `(post ,post)))
    (standard-surrounded-page 
     (list "Edit" *title-separator* (title blog) *title-separator* (title post))
     :content (list (form :method "post" :action (claymore.routing:handler-url 'update-post 'blog blog 'post post)
			  (table (tr
				  (td "title") (td (text-field "TITLE" nil :value (title post))))
				 (tr
				  (td "content") (td (textarea :name "CONTENT" :id "textarea1" :rows "20" :cols "60" (content post))))
				 (tr
				  (td (submit-button :value "update"))))))
     :scripts '(:wysiwyg)
     :operations `(,(link-to-page "back" 'blog-page `(blog ,blog))))))

(defpage update-post (title content post logged-in-user)
  (let* ((blog (with-db (blog post)))
	 (owner (with-db (owner blog))))
    (unless (eql (id owner) (id logged-in-user))
      (redirect-to-page 'post-page :page-options `(post ,post))))
  (setf (title post) title)
  (setf (content post) content)
  (update-modified-post post)
  (redirect-to-page 'post-page :page-options `(post ,post)))

(defpage post-page (post logged-in-user)
  (let* ((blog (with-db (blog post)))
	 (owner (with-db (owner blog))))
    (standard-surrounded-page
     (list (title blog) *title-separator* (title post))
     :content (list (content post)
		    (list-comments post)
		    (if logged-in-user 
			(list (h2 "Add a comment")
			      (new-comment-box post blog))
			(h2 "Login to post a comment")))
     :operations (let ((base (link-to-page (concatenate 'string "blog: " (title blog)) 'blog-page `(blog ,blog))))
		   (if (and logged-in-user (eql (id owner) (id logged-in-user)))
		       (list base (link-to-page "Edit" 'edit-post `(post ,post)))
		       (list base))))))
		       
		       
			   

;;;;;;;;;;;;;;;;;;;
;; comments
(defun new-comment-box (post blog)
  (form :method "post" :action (claymore.routing:handler-url 'create-comment 'post post 'blog blog)
	(table (tr
		(td "title") (td (text-field "TITLE" nil)))
	       (tr
		(td "content") (td (textarea :name "CONTENT" :id "textarea1" :rows "20" :cols "60" "")))
	       (tr (td (submit-button :value "comment!"))))))

(defun list-comments (post)
  (ul :class "comments"
      (loop for comment in (get-comments-for-post post) collect
	   (li (show-comment comment)))))

(defun show-comment (comment)
  (let ((owner (with-db (owner comment))))
    (div :class "comment"
	 (span :class "owner" (nick owner))
	 (h1 (title comment))
	 (p (content comment)))))

(defpage create-comment (title content post logged-in-user)
  (create-new-comment post title content logged-in-user)
  (hunchentoot:redirect (hunchentoot:referer)))
