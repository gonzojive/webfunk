(defpackage :metafunk
  (:use :cl :webfunk))
;  (:export #:start-server))

(in-package :metafunk)

(webfunk:web-defpackage :metafunk
  (:lisp-packages :metafunk))

(webfunk:in-web-package :metafunk)

(webfunk:web-defun index ()
  (hunchentoot:escape-for-html
   (format nil "Available webfunk packages: ~{~A ~}" 
	   (mapcar #'webfunk:web-package-name webfunk:*web-packages*))))
		   
