(defpackage org.iodb.webfunk.metafunk
  (:use #:cl #:asdf))

(in-package :org.iodb.webfunk.metafunk)

(defsystem metafunk
  :description "An ACID-compliant, persistent, and eventually distributed heap."
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "LGPL"
  :components ((:module "src"
			:components
			((:file "metafunk")
	       )))
  :depends-on ("webfunk"))