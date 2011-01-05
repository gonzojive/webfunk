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
			((:file "package")
			 (:file "webfunk" :depends-on ("package"))
			 (:file "routing" :depends-on ("webfunk"))
			 (:file "http" :depends-on ("package" "webfunk" "routing"))
;			 (:file "files" :depends-on ("package"))
;			 (:file "log" :depends-on ("files" "package"))
;			 (:file "heap" :depends-on ("log"))
			 ;;(:file "sails" :depends-on ("package"))
	       )))
  :depends-on ("hunchentoot" "cl-ppcre" "closer-mop" "parse-number" "anaphora" "alexandria" "puri"
                             "cl-json"))


(defsystem webfunk-scripts
  :components ((:module "script"
                        :components ((:file "gen-project"))))
  :depends-on ("webfunk" "cl-emb"))

(defsystem webfunk-tests
  :components ((:module "test"
                        :components ((:file "test-package")
				     (:file "tests" :depends-on ("test-package"))
				     )))
  :depends-on ("webfunk" "stefil"))
