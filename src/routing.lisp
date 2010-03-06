(in-package :webfunk)

;;;; The webfunk routing algorithm is as follows:
;;;;
;;;; 1) Start out with a URI path that looks like /this/is/the-path?asdfasdf=xcf
;;;; 2) Call each of the web packages to see if they should explicitly deal with the request.
;;;;     i) a package will return the portion of the path that specifies some resource
;;;;        below in the hierarchy.  e.g. is/the-path
;;;; 3) 

(defun dispatch-web-functions (request)
  "This is a dispatcher which returns the appropriate handler defined with
WEB-DEFUN, if there is one."
  (webfunk-hunchentoot-dispatcher request))

(defun webfunk-hunchentoot-dispatcher (request)
  (dolist (web-package *web-packages*)
    (multiple-value-bind (matched-name rest-of-uri)
	(web-package-handles-request? web-package request)
      (when matched-name
	(return-from webfunk-hunchentoot-dispatcher
	  #'(lambda ()
	      (web-package-handle-request web-package request :rest-of-uri rest-of-uri))))))
  
  (let ((parsed-path (puri:uri-parsed-path (puri:uri (hunchentoot:request-uri request)))))
    (if *root-web-package*
	#'(lambda () (web-package-handle-request *root-web-package* request :rest-of-uri (cdr parsed-path)))
	#'(lambda () (format nil "<html><head><title>WebFunk web site</title></head><body>Web packages: ~{ ~A ~}</body></html>"
			     (mapcar #'(lambda (x)
					 (format nil "<a href='/~A'>~A</a>" (web-package-uri x)
						 (string-downcase (string (web-package-name x)))))
				     *web-packages*))))))

(defmethod web-package-handles-request? ((web-package web-package) request)
  (let ((uri (hunchentoot:request-uri request)))
    (web-package-prefix-matches-uri? web-package uri)))

(defgeneric web-package-prefix-matches-uri? (web-package uri)
  (:documentation "Determines whether a given web package responds
explicitly to the given URI.  Returns 2 values:

1. NIL if does not match, the matched prefix if it does match.

2. IF it does match, the remaining parsed segments of the URI, as
parsed by PURI."))

(defgeneric web-function-call-with-request (web-function request &key additional-arguments &allow-other-keys)
  (:documentation "Calls the web-function in the context of a web
request.  Passes ADDITIONAL-ARGUMENTS to the function (using
APPLY)."))

(defgeneric  web-function-transform-request-into-arguments (fn request  &key additional-arguments &allow-other-keys)
  (:documentation "Given a web-defun and a request, transforms the
request into a list of arguments to be passed to the function.  Also
passes ADDITIONAL-ARGUMENTS to the function."))

(defmethod web-package-prefix-matches-uri? ((web-package web-package) uri)
  (let* ((puri (puri:uri uri))
	 (parsed-path (puri:uri-parsed-path puri))
	 (package-alias (the (or null string) (and parsed-path (second parsed-path))))
	 (matching-prefix (when package-alias
			    (find-if #'(lambda (package-uri)
					 (equalp package-alias package-uri))
				     (cons (web-package-uri web-package)
					   (web-package-uri-aliases web-package))))))
    (values matching-prefix
	    (when matching-prefix
	      (nthcdr 2 parsed-path)))))

(defmethod web-package-handle-request ((web-package web-package) request &key rest-of-uri &allow-other-keys)
  (multiple-value-bind (fn new-rest-of-uri)
      (let ((matched-fn (or (find (car rest-of-uri)
                                  (web-package-functions web-package)
                                  :key (compose #'symbol-name #'web-function-name)
                                  :test #'equalp)
                            (awhen (and (equal "" (car rest-of-uri))
                                        (web-package-root-function-name web-package))
                              (find it (web-package-functions web-package) :key #'web-function-name)))))
	(if matched-fn
	    (values matched-fn (cdr rest-of-uri))
	    (values (web-package-default-function-name web-package) rest-of-uri)))
    (let ((*web-package* web-package))
      (if fn
	  (web-function-call-with-request fn request :additional-arguments (list :rest-of-uri new-rest-of-uri))
	  (format nil "No web-function found for '~A'. (Options are ~{'~A' ~})"
		  (car rest-of-uri)
		  (mapcar #'(lambda (fn) (hunchentoot:escape-for-html (string-downcase (string (web-function-name fn)))))
			  (web-package-functions web-package)))))))


;(defmacro with-temporarily-interned-symbol ((var string &optional (package nil package-p)) &body body)
;  (let ((symbol-existed-p-var (gensym "symbol-existedp"))
;	(string-var (gensym "string"))
;	(package-var (gensym "package")))
;    `(let ((,string-var ,string)
;	   (,package-var ,package))
;       (multiple-value-bind (,var ,symbol-existed-p-var)
;	 ,(if package `(intern string package) `(intern string))
;       (multiple-value-prog1
;	   (progn ,@body)
;	 (when (not ,symbol-existed-p-var)
;	   (unintern ,var 

;(defparameter *wiretap* *standard-output*)

(defmethod web-function-transform-request-into-arguments ((fn web-function) request &key additional-arguments &allow-other-keys)
  `(:called-from-web?
    t
    ,@additional-arguments
    ,@(mapcan #'(lambda (fn-param)
		  (list (intern (symbol-name (parameter-name fn-param)) :keyword)
			(cond
			  ((hunchentoot:parameter (parameter-uri-name fn-param))
			   (compute-parameter (parameter-uri-name fn-param)
					      (or (parameter-type fn-param) 'string)
					      :both))
			  ((parameter-init-form-function fn-param)
			   (funcall (parameter-init-form-function fn-param))))))
	      (web-function-parameters fn))))
    


(defmethod web-function-call-with-request ((fn web-function) request &key additional-arguments &allow-other-keys)
  (apply fn (web-function-transform-request-into-arguments fn request :additional-arguments additional-arguments)))

;  (loop :for (uri server-names easy-handler) :in *easy-handler-alist*
;     :when (and (or (eq server-names t)
;		   (find (server-name *server*) server-names :test #'eq))
;	       (cond ((stringp uri)
;		      (string= (script-name request) uri))
;		     (t (funcall uri request))))
;     :do (return easy-handler)))



;(defclass web-resource ()
;  ((child-alist :initform nil :accessor resource-child-alist :initarg :child-alist
;		:documentation "A list where CAR is a name and CDR is another resource.")))

;(defgeneric child-resource (resource uri-path &key request &allow-other-keys)
;  (:documentation "Returns the child resource that"))

;(defgeneric web-render (resource
  
