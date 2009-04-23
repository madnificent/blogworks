(in-package :migration-user)

(setf *db-connection-parameters* '("blogworks" "blogger" "dablog" "localhost"))
(def-query-migration 1240261019 "Adding users"
  :execute "CREATE TABLE blog_user ( user_id SERIAL PRIMARY KEY, nick text, email text, password text )"
  :revert  "DROP TABLE blog_user")

(def-query-migration 1240261363 "Adding blogs"
  :execute "CREATE TABLE blog ( blog_id SERIAL PRIMARY KEY, owner INTEGER REFERENCES blog_user, title text, description text )"
  :revert "DROP TABLE blog")

(def-query-migration 1240261518 "Adding posts"
  :execute "CREATE TABLE post ( post_id SERIAL PRIMARY KEY, blog INTEGER REFERENCES blog, title text, content text )"
  :revert "DROP TABLE post")

(def-query-migration 1240262433 "Adding comments"
  :execute "CREATE TABLE comment ( comment_id SERIAL PRIMARY KEY, owner INTEGER REFERENCES blog_user, post INTEGER REFERENCES post, title text, content text )"
  :revert "DROP TABLE comment")

(def-queries-migration 1240517894 "Adding database model checks for blog_user"
  :execute ("ALTER TABLE blog_user ADD CONSTRAINT unique_nickname UNIQUE(nick)"
	    "ALTER TABLE blog_user ADD CONSTRAINT unique_email UNIQUE(email)"
	    "ALTER TABLE blog_user ADD CONSTRAINT sane_email CHECK (email LIKE '%@%.%')"
	    "ALTER TABLE blog_user ADD CONSTRAINT sane_passwd CHECK (password LIKE '_____%')"
	    "ALTER TABLE blog_user ADD CONSTRAINT valid_passwd CHECK (password != nick)")
  :revert ("ALTER TABLE blog_user DROP CONSTRAINT unique_nickname"
	   "ALTER TABLE blog_user DROP CONSTRAINT unique_email"
	   "ALTER TABLE blog_user DROP CONSTRAINT sane_email"
	   "ALTER TABLE blog_user DROP CONSTRAINT sane_passwd"
	   "ALTER TABLE blog_user DROP CONSTRAINT valid_passwd"))
