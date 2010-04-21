(in-package :blogworks.setup)

(setf (cw.setup.helpers:db-dir) "/tmp/blogworks/db/") ;; tmp really isn't the best path to choose here, however it should work out of the box

(setf hunchentoot:*message-log-pathname* "/tmp/blogworks/blog.msg.log")
(setf hunchentoot:*access-log-pathname* "/tmp/blogworks/blog.accs.log")
(start-server)

;; This is the library that can be found at http://www.openwebware.com/
(defparameter *wysiwyg-path* "/home/madnificent/code/lisp/blogworks/openwysiwyg/")
;; This is part of the content blogworks ships with
(defparameter *css-path* "/home/madnificent/code/lisp/blogworks/css/")
(defparameter *google-analytics-key* nil
  "Set this to add google analytics support.  
   This should be set to a string that contains your google analytics key.
   An example of this is \"UA-846893-4\" (this key has been chosen at random and probably doesn't exist)")
(defparameter *recaptcha-p* T "Set this to T if you want to check users etc using recaptcha")
(setf cl-recaptcha:*private-captcha-key* "YOUR_KEY"); "You can get your key from recaptcha.com"
(setf cl-recaptcha:*public-captcha-key* "YOUR_KEY"); "You can get your key from recaptcha.com"
