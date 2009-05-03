(in-package :blogworks.setup)

(setf database-migrations:*db-connection-parameters* '("blogworks" "blogger" "dablog" "localhost"))
(setf hunchentoot:*message-log-pathname* "/tmp/blog.msg.log")
(setf hunchentoot:*access-log-pathname* "/tmp/blog.accs.log")
(defvar server-online? nil "Did we start the server yet?")
(unless server-online?
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080))
  (setf server-online? T))
;; This is the library that can be found at http://www.openwebware.com/
(defparameter *wysiwyg-path* "/home/madnificent/code/lisp/sites/blogworks/openwysiwyg/")
;; This is part of the content blogworks ships with
(defparameter *css-path* "/home/madnificent/code/lisp/sites/blogworks/css/")
(defparameter *google-analytics-key* nil
  "Set this to add google analytics support.  
   This should be set to a string that contains your google analytics key.
   An example of this is \"UA-846893-4\" (this key has been chosen at random and probably doesn't exist)")