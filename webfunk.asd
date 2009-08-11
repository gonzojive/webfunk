(defpackage org.iodb.webfunk
  (:use #:cl #:asdf))

(in-package :org.iodb.webfunk)

(defsystem webfunk
  :description "An ACID-compliant, persistent, and eventually distributed heap."
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "LGPL"
  :components ((:module "src"
			:components
			((:file "webfunk")
;			 (:file "files" :depends-on ("package"))
;			 (:file "log" :depends-on ("files" "package"))
;			 (:file "heap" :depends-on ("log"))
			 ;;(:file "sails" :depends-on ("package"))
	       )))
  :depends-on ("hunchentoot" "cl-ppcre" "closer-mop" "parse-number"))

;(defsystem persistent-heap-tests
;  :components ((:module "test"
;                        :components ((:file "test-package")
;				     (:file "log-tests" :depends-on ("test-package"))
;				     )))
;  :depends-on ("persistent-heap" "stefil"))
