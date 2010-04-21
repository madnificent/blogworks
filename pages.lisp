;;;; TODO: check wether or not the logged-in-user is the correct user for blog creation etc

(in-package :blogworks.routing)

(claymore.routing::defhandles as-failsafe-for (failsafe &rest other-cases)
  "Allows you to create a failsafe option for simple cases. Only expands into cases that start by stealing a part of the url (like \"new\" or \"login\")"
  `(,@(map 'list (lambda (subroute) 
		   (if (and (listp subroute) (stringp (first subroute)))
		       `(,(first subroute) (handles ,failsafe as-failsafe-for ,@(rest subroute)))
		       subroute))
	   other-cases)
      `("*" ,failsafe))) ;; TODO: /me has headache now, perhaps the routing system should allow for cleaner expansions :)  RE: It isn't all that hard though.  It just looks ugly at first sight.

(def-identification-handler blog-user #'nickname #'find-user-by-nickname)
(def-identification-handler blog #'name #'find-blog-by-name)
;; (def-identification-handler post nil nil)  ;; TODO: it should be possible to identify a post by both the blog and the title of the post.  The routing system must support this in the future (I THINK IT IS POSSIBLE, IT SHOULD BECOME DOCUMENTED THOUGH)

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
		      ("login" (when post-request login)
		       ("unknown-user" unknown-user-login))
		      ("logout" (when post-request logout))
		      ("users"
		       (when logged-in-user?
			 ("edit" (when get-request edit-user-page)
				 (when post-request update-user-page))
			 personal-user-page)
		       (".*" (handler identifies blog-user as user)
			     public-user-page))
		      ("blogs"
		       (when logged-in-user? 
			 ("new" (when get-request new-blog)
				(when post-request create-blog))
			 ("delete" (when post-request delete-blog)))
		       (".*" (handler identifies blog as blog)
			     blog-page
			     ("delete"
			      (when logged-in-user?
				(when post-request delete-blog-page)))
			     ("posts"
			      (when logged-in-user?
				(when get-request new-post)
				(when post-request create-post))
			      (".*" (handler sets post-title)
				    post-page
				    ("edit" (when get-request edit-post)
					    (when post-request update-post))
				    ("comments" (when logged-in-user? (when post-request create-comment))))))))))

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
(defun google-analytics () 
  (when blogworks.setup:*google-analytics-key*
    (list (script :type "text/javascript" 
		  "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");"
		  "document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));")
	  (script :type "text/javascript"
		  "try {"
		  "var pageTracker = _gat._getTracker(\"" blogworks.setup:*google-analytics-key* "\" );"
		  "pageTracker._trackPageview();"
		  "} catch(err) {}"))))
			       

(defun standard-surrounded-page (subtitle &key operations content scripts)
  (html (head
	 (>:title *title-prefix* *title-separator* subtitle)
	 (link :rel "stylesheet" :type "text/css" :href "/css/base.css")
	 (loop for script in scripts collect
	      (getf *scripts* script)))
	(body (div :class "header" (h1 subtitle))
	      (div :class "content" content)
	      (div :class "operations" 
		   (link-to-page "Home" 'welcome) 
		   (loop for o in operations collect (list *link-separator* o))
		   *link-separator* (div :class "login" (user-login-operations)))
	      (google-analytics))))


(defun logged-in-user ()
  (claymore:param* 'logged-in-user))

(defdirector redirect-to-welcome ()
  (redirect-to-page 'welcome))

(defcontroller welcome ()
  (flow:forward (logged-in-user)))
(defview welcome (logged-in-user)
  (standard-surrounded-page
   "Welcome"
   :content (list (p "Hello user, welcome to the blogworks blogging platform.  Feel free to login, create an account or sniff around.")
		  (new-users)
		  (new-blogs)
		  (new-posts))
   :operations (unless logged-in-user (list (link-to-page "new user" 'new-user)))))

;;;;;;;;;;;;;;;;;;;
;; login
(defun user-login-operations (&optional (logged-in-user (claymore:param* 'logged-in-user)))
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

(defcontroller login ()
  (let ((user (find-user-by-name-and-password (claymore:param* 'nick) (claymore:param* 'password))))
    (when user
      (setf (hunchentoot:session-value 'logged-in-user) user))
    (flow:forward user)))
(defdirector login (user)
  (if user
      (hunchentoot:redirect (hunchentoot:referer))
      (redirect-to-page 'unknown-user-login)))

(defview unknown-user-login ()
  (standard-surrounded-page
   "User/Password combination was incorrect"
   :content (list (p "We're sorry, the user which you specified was unknown, please try to login again"))))

(defun logout-user ()
  (declare (special *session*)) ;; TODO: flow must support declare too
  (hunchentoot:remove-session hunchentoot:*session*))

(defcontroller logout ()
  (logout-user)
  (flow:forward "foobar"))
(defdirector logout ()
  (redirect-to-page 'welcome))

;;;;;;;;;;;;;;;;;;;
;; users
(defun new-users (&optional (amount 10))
  (div :class "stat"
       (h1 "new users")
       (ul (loop for user in (latest-users amount) collect
		(li (link-to-page (nickname user) 'public-user-page `(user ,user)) " :: " (email-address user))))))

(defcontroller new-user ()
  (when (logged-in-user)
    (logout-user)))
(defview new-user ()
  (standard-surrounded-page 
   "New User"
   :content (list (p "please fill out the data below to create a new user")
		  (form :method "post" :action (claymore.routing:handler-url 'create-user)
			(text-field "NICKNAME" T) (br)
			(strong "PASSWORD") (>:input :type "password" :name "PASSWORD") (br)
			(text-field "EMAIL" T) (br)
			(when blogworks.setup:*recaptcha-p* (cl-recaptcha:challenge))
			(submit-button :value "create")))))

(defcontroller-t create-user ()
  (apply (lambda (nickname password email recaptcha_challenge_field recaptcha_response_field)  ;; TODO: Yuck, ugly construction
	   (if (or (not blogworks.setup:*recaptcha-p*)
		   (cl-recaptcha:verify-captcha recaptcha_challenge_field recaptcha_response_field (hunchentoot:remote-addr*)))
	       (make-instance 'user :nick nickname :pass password :email email)))
	 (mapcar #'claymore:param* '(nickname password email recaptcha_challenge_field recaptcha_response_field))))

(defdirector create-user ()
  (redirect-to-page 'welcome))

(defcontroller personal-user-page ()
  (flow:forward (logged-in-user)))
(defdirector personal-user-page (user)
  (unless user
    (redirect-to-page 'welcome))
  (flow:forward user))
(defview personal-user-page (user)
  (p-user-page :user user :logged-in-user user))

(defun p-user-page (&key user logged-in-user)
  (standard-surrounded-page
   (list "user page for " (nickname user))
   :content (list (h2 "Blogs of " (nickname user))
		  (mapcar (lambda (blog) (link-to-page (title blog)
						       'blog-page `(blog ,blog)))
			  (blogs-of-user user)))
   :operations (if (and logged-in-user (eql user logged-in-user))
		   (list (link-to-page "New blog" 'new-blog)
			 (link-to-page "Edit user" 'edit-user-page)))))

(defcontroller public-user-page ()
  (flow:forward (claymore:param* 'user)))
(defview public-user-page (user)
  (p-user-page :user user))

(defcontroller edit-user-page ()
  (flow:forward (logged-in-user)))
(defview edit-user-page (logged-in-user)
  (standard-surrounded-page 
   (list "Edit user :: " (nickname logged-in-user))
   :content (list (p "Please enter the new info of the user.  Any data that is left empty will not be set.")
		  (form :method "post" :action (claymore.routing:handler-url 'update-user-page)
			(strong "password") (claymore.html.full:input :type "password" :name "password") (br)
			(strong "password confirmation") (claymore.html.full:input :type "password" :name "password_confirmation") (br)
			(text-field "email" T :value (email-address logged-in-user)) (br)
			(submit-button :value "update")))))

(defcontroller-t update-user-page ()
  (let ((new-email (claymore:param* 'email))
	(new-password (claymore:param* 'password))
	(user (logged-in-user)))
    (unless (string= "" new-email)
      (setf (email-address user) new-email))
    (unless (or (string= "" new-password) (not (string= new-password (claymore:param* 'password_confirmation))))
      (setf (password user) new-password))))
(defdirector update-user-page ()
  (redirect-to-page 'personal-user-page))

;;;;;;;;;;;;;;;;;;;
;; blogs
(defun blogs-for-user (user)
  (ul :class "blogs"
      (loop for blog in (blogs-of-user user) collect
	   (li (link-to-page (title blog) 'blog-page `(blog ,blog))))))

(defun new-blogs (&optional (amount 10))
  (div :class "stat"
       (h1 "latest blogs")
       (ul (loop for blog in (latest-blogs amount) collect
		(li (link-to-page (title blog) 'blog-page `(blog ,blog)))))))

(defcontroller new-blog ()
  (flow:forward (logged-in-user)))
(defview new-blog (logged-in-user)
  (standard-surrounded-page 
   "New Blog"
   :content (list (h1 "Create a new blog for " (nickname logged-in-user))
		  (p "Please enter the required data about the blog below")
		  (form :method "post" :action (claymore.routing:handler-url 'create-blog)
			(table (tr
				(td "name") (td (text-field "NAME" nil)))
			       (tr
				(td "title") (td (text-field "TITLE" nil)))
			       (tr
				(td "description") (td (textarea :name "DESCRIPTION" :id "textarea1" :rows "20" :cols "60" "")))
			       (tr
				(td (submit-button :value "create"))))))
   :scripts '(:wysiwyg)))

(defcontroller-t create-blog ()
  (when (logged-in-user)
    (let ((title (claymore:param* 'title))
	  (description (claymore:param* 'description))
	  (name (claymore:param* 'name)))
      (make-instance 'blog :owner (logged-in-user) :title title :description description :name name))))
(defdirector create-blog ()
  (redirect-to-page 'welcome))

(defcontroller blog-page ()
  (let ((blog (claymore:param* 'blog)))
    (flow:forward blog (owner blog) (logged-in-user))))

(defview blog-page (blog owner logged-in-user)
  (standard-surrounded-page
   (list "Blog" *title-separator* (title blog))   
   :content (list (p (description blog))
		  (if (posts-in-blog blog)
		      (list (h2 "Posts in " (title blog))
			    (posts-for-blog blog))
		      (h2 "No posts for " (title blog))))
   :operations (if (and logged-in-user (eql owner logged-in-user))
		   (list (link-to-page "new post" 'new-post `(blog ,blog))
		   	 (bttn-to 'delete-blog-page "delete blog" :url-options (list 'blog blog)))
		   (list (link-to-page (nickname owner) 'public-user-page `(user ,owner))))))

(defcontroller-t delete-blog-page ()
  (let ((blog (claymore:param* 'blog)))
    (when (and blog (logged-in-user)
	       (eq (owner blog) (logged-in-user)))
      (remove-object blog))))
(defdirector delete-blog-page ()
  (redirect-to-page 'personal-user-page))

;;;;;;;;;;;;;;;;;;;
;; posts
(defun new-posts ()
  (div :class "stat"
       (h1 "Latest posts")
       (ul (loop for post in (latest-posts 10) collect
		(li (link-to-page (title post)
				  'post-page `(blog ,(blog post) post-title ,(title post))))))))

(defun posts-for-blog (blog)
  (ol :class "posts"
      (loop for post in (posts-in-blog blog) collect
	   (li (link-to-page (title post) 'post-page `(blog ,blog post-title ,(title post)))))))

(defcontroller new-post ()
  (flow:forward (claymore:param* 'blog)))
(defview new-post (blog)
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
  
(defcontroller-t create-post ()
  (let ((post (make-instance 'post :blog (claymore:param* 'blog) :title (claymore:param* 'title) :content (claymore:param* 'content))))
    (flow:forward post)))
(defdirector create-post (post)
  (redirect-to-page 'post-page :page-options `(post-title ,(title post) blog ,(blog post))))

(defcontroller edit-post ()
  (let ((blog (claymore:param* 'blog))
	(title (claymore:param* 'post-title)))
    (flow:forward (find-post-by-blog-and-title blog title)
		  (logged-in-user)
		  blog
		  (owner blog))))
(defdirector edit-post (post logged-in-user blog owner)
  (unless (eq owner logged-in-user)
    (redirect-to-page 'post-page :page-options `(post ,post)))
  (flow:forward post logged-in-user blog owner))
(defview edit-post (post logged-in-user blog owner)
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
     :operations `(,(link-to-page "back" 'blog-page `(blog ,blog))))  )

(defcontroller-t update-post ()
  (let* ((title (claymore:param* 'title))
	 (content (claymore:param* 'content))
	 (blog (claymore::param* 'blog))
	 (post-title (claymore::param* 'post-title))
	 (post (find-post-by-blog-and-title blog post-title))
	 (owner (owner blog)))
    (when (eq owner (logged-in-user))
      (setf (title post) title)
      (setf (content post) content))
    (flow:forward blog post-title)))
(defdirector update-post (blog post-title)
  (redirect-to-page 'post-page :page-options `(blog ,blog post-title ,post-title)))

(defcontroller post-page ()
  (let* ((blog (claymore:param* 'blog))
	 (post (find-post-by-blog-and-title blog (claymore:param* 'post-title)))
	 (owner (owner blog)))
    (flow:forward post owner blog (logged-in-user))))
(defview post-page (post owner blog logged-in-user)
  (standard-surrounded-page
   (list (title blog) *title-separator* (title post))
     :content (list (content post)
		    (list-comments post)
		    (if logged-in-user 
			(list (h2 "Add a comment")
			      (new-comment-box post blog))
			(h2 "Login to post a comment")))
     :operations (let ((base (link-to-page (concatenate 'string "blog: " (title blog)) 'blog-page `(blog ,blog))))
		   (if (and logged-in-user (eq owner logged-in-user))
		       (list base (link-to-page "Edit" 'edit-post `(post-title ,(title post) blog ,blog)))
		       (list base)))))

;;;;;;;;;;;;;;;;;;;
;; comments
(defun new-comment-box (post blog)
  (form :method "post" :action (claymore.routing:handler-url 'create-comment 'post-title (title post) 'blog blog)
	(table (tr
		(td "title") (td (text-field "TITLE" nil)))
	       (tr
		(td "content") (td (textarea :name "CONTENT" :id "textarea1" :rows "20" :cols "60" "")))
	       (tr (td (submit-button :value "comment!"))))))

(defun list-comments (post)
  (ul :class "comments"
      (loop for comment in (comments-on post) collect
	   (li (show-comment comment)))))

(defun show-comment (comment)
  (let ((owner (owner comment)))
    (div :class "comment"
	 (span :class "owner" (nickname owner))
	 (h1 (title comment))
	 (p (content comment)))))

(defcontroller-t create-comment ()
  (let* ((blog (claymore:param* 'blog))
	 (post (find-post-by-blog-and-title blog (claymore:param* 'post-title))))
    (make-instance 'comment :parent post :title (claymore:param* 'title) :content (claymore:param* 'content) :owner (logged-in-user))
    (flow:forward blog post)))
(defdirector create-comment (blog post)
  (redirect-to-page 'post-page :page-options `(blog ,blog post-title ,(title post))))
