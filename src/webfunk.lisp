(defpackage :webfunk
  (:use :common-lisp)
  (:export 
   ;; macros
   #:web-defun
   ;; web packages
   #:web-defpackage #:webpackage
   #:*web-packages*
   #:*web-package*
   #:web-package
   #:find-web-package
   #:in-web-package
   #:web-package-name
   #:web-package-functions
   ;; web functions
   #:web-function

   #:web-function-href
   #:href

   ;; serving files
   #:serve-static-file
   
   ;; hunchentoot plug-in
   #:webfunk-hunchentoot-dispatcher))

(in-package :webfunk)

;(defvar *web-package-table* (make-hash-table :test #'equal)
;  "Hash table of strings to WEB-PACKAGE objects.")

(defvar *web-packages* nil
  "List of WEB-PACKAGE objects.")

(defvar *web-package* nil
  "Active web package.  Set by IN-WEB-PACKAGE.")

(defvar *lisp-package-web-package-alist* nil
  "AList to map lisp packages to web packages (many to one).")

(defmacro in-web-package (web-package-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *web-package* (find-web-package ,web-package-designator))))

;;; By default, web packages are located at "/WEB-PACKAGE-NAME/"
;;; and the URL of a WEB-FUNCTION is
;;;  "/WEB-PACKAGE-NAME/WEB-FUNCTION-NAME"
(defgeneric web-package-uri (web-package)
  (:documentation "Returns the URI path (e.g. /path/) that is basis
for the web package."))

(defgeneric web-package-handles-request? (web-package request)
  (:documentation "Returns T if the given web package definitely handles the request.
Otherwise returns NIL"))

(defgeneric web-package-handle-request (web-package request)
  (:documentation "Returns a string as the result of the given HTTP request."))

(defclass web-package ()
  ((name
    :type string
    :initarg :name
    :accessor web-package-name
    :documentation "The name of the package.")
   (uri
    :type (or null string)
    :initform nil
    :initarg :uri
    :accessor web-package-uri
    :documentation "The canonical base URI for the package.")
   (uri-aliases
    :type list
    :initform nil
    :initarg :uri-aliases
    :accessor web-package-uri-aliases
    :documentation "Secondary URIs used to locate the web package.")
   (root-function-name
    :type (or null symbol)
    :initform nil
    :initarg :root-function :initarg :root-function-name
    :accessor web-package-root-function-name
    :documentation "When the user navigates to the root a directory, this is the function name to call.")
   (functions
    :initform nil
    :initarg :functions
    :accessor web-package-functions))
  (:documentation "A WEB-PACKAGE is a named collection of functions."))

(defmethod print-object ((object web-package) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (web-package-name object))))

(defun find-web-package (name)
  "Finds the WEB-PACKAGE of the given name."
  (when (not (stringp name)) (setf name (string name)))
  (find name *web-packages* :key #'web-package-name :test #'string-equal))

(defun find-web-function (web-package fn-name)
  "Returns the web function with the given symbol as its name in the given web package"
  (find fn-name (web-package-functions web-package) :key #'web-function-name))

(defclass web-function (standard-generic-function)
  ((web-package :accessor web-package :initform nil :initarg :web-package)
   (web-param :accessor web-function-parameters :initform nil :initarg :web-parameters))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "A function designed to be called over the web via webfunk."))

(defgeneric web-function-name (web-function)
  (:documentation "Returns the symbol that names a particular web function.")
  (:method ((web-function web-function))
    (closer-mop:generic-function-name web-function)))

(defgeneric (setf web-function-name) (web-function name)
  (:documentation "Sets the symbol that names a particular web function.")
  (:method ((web-function web-function) name)
    (setf (closer-mop:generic-function-name web-function) name)))

;(defclass web-generic-function-class (standard-generic-function)
;  ()
;  (:documentation "web-generic-function-"))

(defclass web-function-parameter ()
  ((symbol
    :type symbol
    :initform nil
    :initarg :name :initarg :symbol
    :accessor parameter-symbol :accessor parameter-name
    :documentation "Canonical name of the argument")
   (uri-name 
    :type (or string null)
    :initform nil
    :initarg :uri-name
    :accessor parameter-uri-name
    :documentation "The string that names this variable in a URL.")
   (parameter-type
    :initform nil
    :initarg :parameter-type
    :accessor parameter-type
    :documentation "It's unclear exctly what this is at the moment.")
;   (parameter-string-converter
;    :documentation "Function that will convert a string before passing it to this function."
;    :initform #'identity)
   (init-form-function
    :initform #'(lambda () nil)
    :type (or function null)
    :initarg :initform-function
    :accessor parameter-init-form-function))
  (:documentation "A parameter definition in a web-function."))

(defmethod print-object ((object web-function-parameter) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (parameter-name object))))

(defmethod initialize-instance ((package web-package) &rest rest &key name (uri nil uri-key-p) &allow-other-keys)
  "Initializer for web package.  If URI is not set, sets it to downcased NAME + '/'"
  
  (declare (ignore uri))
  (if uri-key-p
      (call-next-method)
      (apply #'call-next-method package
	     :uri (format nil "~A/" (string-downcase name)) ;(web-package-name package)))
	     rest)))

(defmethod shared-initialize ((package web-package) slot-names &rest rest &key name (uri nil uri-key-p) lisp-packages &allow-other-keys)
  "Initializer for web package.  If URI is not set, sets it to downcased NAME + '/'"
  (declare (ignore uri))
  (Format t "Lisp packages: ~S~%Rest: ~S~%" lisp-packages rest)
  (if uri-key-p
      (call-next-method)
      (apply #'call-next-method package slot-names
	     :uri (format nil "~A/" (string-downcase name)) ;(web-package-name package)))
	     rest)))

(defgeneric reinitialize-web-package (web-package &key)
  (:documentation "Called when WEB-DEFPACKAGE defines a package that already
exists and the information must be SETF into the new package.")
  (:method ((package web-package) &rest rest
	     &key package-class
	    &allow-other-keys)
    (if (and package-class (not (eql package-class (class-of package))))
	(apply #'change-class package package-class rest)
	(apply #'reinitialize-instance package rest))))

(defgeneric web-package-class-initarg-singular-p (web-package-class initarg)
  (:documentation "Returns T if the keyword option INITARG in a defpackage form
should be handed to (make-instance WEB-PACKAGE-CLASS) as a single value,
rather than a list.")
  (:method ((web-package-class (eql (find-class 'web-package))) initarg)
    (case initarg
      ((or :uri :listed :root-function-name :root-function) t)
      (t nil))))

(defgeneric web-package-class-initarg-evaluated-p (web-package-class initarg)
  (:documentation "Returns T if the keyword option INITARG in a defpackage form
should be handed to (make-instance WEB-PACKAGE-CLASS) evaluated or not evaluated.")
  (:method ((web-package-class (eql (find-class 'web-package))) initarg)
    (case initarg
      ((or :uri :listed :root-function-name :root-function) nil)
      (t nil))))

(defun maphash-ret (fn hash-table)
  (let ((result nil))
    (maphash #'(lambda (a b)
		 (push (funcall fn a b) result))
	     hash-table)
    (nreverse result)))
		 

(defun merge-options-by-keyword (options &optional (return-type 'list))
  "Given a list of option forms, which generally looks like ((:a 1 2 3 4) (:b t) (:c 3) (:a 5)),
merges all the options with the same car together.  So it will return something like
 ((:a 1 2 3 4 5) (:b t)).  "
  (let ((hash (make-hash-table))
	(options-in-order nil))

    (map nil #'(lambda (option)
		 (setf options-in-order (concatenate 'list options-in-order (rest option)))
		 (setf (gethash (car option) hash)
		       (concatenate 'list (gethash (car option) hash) (rest option))))
	 options)
    (values (case return-type
	      (list (apply #'concatenate 'list (maphash-ret #'list hash)))
	      (hash-table hash)
	      (t nil))
	    options-in-order)))


(defmacro web-defpackage (defined-package-name &body options)
  "Defines a WEB-PACKAGE.  Options is a list of options.  An option takes the form
OPTION ::= (:uri STRING) | (:uri-aliases STRING*) | (:package-class package-class-name)

:uri specifies the URI/resource locator for the package being defined
:uri-aliases specifies a list of alternative URIs that also identify this package
:package-class designates the class this macro will instantiate, default is WEB-PACKAGE."
  (let ((web-package-class
	 (find-class (or (second (find :package-class options :key #'car))
			 'web-package))))
    (flet ((options-to-make-instance-keys (options)
	     ;; We have to be careful so that things evaluate in the right order.
	     ;; basically option arguments should be evaluated in the order
	     ;; they are seen.
	     (mapcan #'(lambda (option)
			 (let ((singular (web-package-class-initarg-singular-p web-package-class (car option)))
			       (evaluated (web-package-class-initarg-evaluated-p web-package-class (car option))))
			 (list (car option)
			       (if singular
				   (if evaluated (second option) `',(second option))
				   (if evaluated `(list ,@(rest option)) `'(,@(rest option)))))))
				       
					    

				      
			       
;			       `',(funcall (if (web-package-class-initarg-singular-p
;						web-package-class (car option))
;					       #'second
;					       #'rest)
;					   option)))
			 options)))
      (let ((%package (gensym "WEB-PACKAGE"))
	    (package-name (if (stringp defined-package-name)
			      (intern (string-upcase defined-package-name))
			      defined-package-name)))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((,%package (make-web-package ,web-package-class
					      :name ,package-name
					      ,@(options-to-make-instance-keys options))))
	     ,%package))))))

(defmacro webpackage (defined-package-name &body options)
  `(web-defpackage defined-package-name ,@options))

(defun make-web-package (web-package-class &rest rest &key name lisp-packages &allow-other-keys)
  "Creates a web package as if by MAKE-INSTANCE, except it will
not instantiate a new package if one was found with the same name.
In that case, new information will be SETF into the existing
WEB-PACKAGE class by calling REINITIALIZE-WEB-PACKAGE with
all the arguments passed to this function."
;  (format t "WEB-PACKAGE-PARAMS: ~S~%" rest)
  (declare (optimize debug))
  (format t "Args to make-web-package: ~S~%" rest)
  (let ((web-package-maybe (find-web-package name)))
    (let ((web-package (if (null web-package-maybe)
			   (apply #'make-instance web-package-class rest)
			   (apply #'reinitialize-web-package web-package-maybe rest))))

      (setf *web-packages* (pushnew web-package *web-packages*));(remove web-package-maybe *web-packages*)))
      
      (dolist (lisp-package lisp-packages)
	(let ((package (find-package lisp-package)))
	  (pushnew (cons package web-package) *lisp-package-web-package-alist*)))

      (values
	web-package
	(null web-package-maybe)))))

(defmethod initialize-instance :after ((param web-function-parameter) &key &allow-other-keys)
  "Set PARAMETER-NAME to downcased string of the variable's name and
INIT-FORM-FUNCTION"
  ;; initialize the URL parameter name to the downcased version of
  ;; the variable name string
  (when (null (parameter-uri-name param))
    (setf (parameter-uri-name param)
	  (string-downcase (symbol-name (parameter-name param))))))

(defvar *content-type-keyword-string-alist*
  `((:html . "text/html")
    (:css . "text/css")
    (:javascript . "text/javascript")
    (:jpeg . "image/jpeg")
    (:gif . "image/gif")
    (:png . "image/png")))
    

(defun stringify-content-type (content-type)
  "Makes a content type string from content-type, which is either a string
or a keyword symbol.

:html -> text/html
:css  -> text/css
:javascript -> text/javascript"
  (typecase content-type
    (string content-type)
    (symbol (if (keywordp content-type)
		(cdr (assoc content-type *content-type-keyword-string-alist*))
		(error "non-keyword symbol")))
    (t (error "only accepts keywords and strings but got ~S" content-type))))

(defun sane-web-package ()
  (or *web-package*
      (cdr (assoc *package* *lisp-package-web-package-alist*))))

(defmacro web-defun (description web-lambda-list &body body)
;;   "Defines a function intended to be called over the web (though it can be called
;; as if it is a normal function, too.
;; 
;; DESCRIPTION is either a symbol or a list of the form:
;; (name &key content-type)
;; 
;; LAMBDA-LIST is a list where each elemtn is either a symbol VAR or a list of the form:
;; (var &key init-form parameter-name parameter-type)
;; 
;; description := name | name-lambda-list."
  
  ;; Standardize the description argument into the list form
  (when (atom description) (setf description (list description)))
  ;; Standardize the web-lambda-list items into lists
  (setf web-lambda-list (mapcar #'(lambda (x) (if (atom x) (list x) x)) web-lambda-list))
  (destructuring-bind (fn-name &key content-type)
      description
    (let* ((parameter-names (mapcar #'first web-lambda-list))
	   (make-parameters-form
	    `(list ,@(mapcar #'(lambda (param-form)
				 (destructuring-bind (name &key initform parameter-name parameter-type)
				     param-form
				   (declare (ignore initform parameter-name))
				   `(make-instance 'web-function-parameter
						   :name ',name
						   :initform-function #'(lambda () ,initform)
						   :parameter-type ,parameter-type)))
			     web-lambda-list)))

	   (generic-lambda-list (append (list '&key) parameter-names (list '&allow-other-keys)))
	   (method-lambda-list (append (list '&key) parameter-names (list '&allow-other-keys)))

	   (%fn (gensym "function"))
	   (%parameters (gensym "parameters"))
	   (content-type (and content-type (stringify-content-type content-type))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (let* ((*web-package* (sane-web-package))
		(,%parameters ,make-parameters-form)
		(,%fn
		 (defgeneric ,fn-name ,generic-lambda-list 
		   (:generic-function-class web-function))))

	   (setf (web-function-parameters ,%fn) ,%parameters
		 (web-package ,%fn) *web-package*)

	   (defmethod ,fn-name ,method-lambda-list
	     ,@(when content-type
		 (list `(when (boundp 'hunchentoot:*reply*)
			  (setf (hunchentoot:content-type*) ,content-type))))
	     (funcall (lambda () ,@body)))
	   (pushnew ,%fn (web-package-functions *web-package*))
	   ,%fn)))))
       ;; unfinished implementation

(defun string-prefixp (string prefix)
  "Returns T if STRING begins with the same characters as are in prefix."
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

(defmethod web-package-prefix-matches-uri? ((web-package web-package) uri)
  "Returns 2 values.  1. nil if does not match, the matched prefix if it does match.
2.  if it does match, the rest of the uri string."
  (cl-ppcre:register-groups-bind (firstlevel slash secondlevel postfix)
				 ("/([^\\/]*)(\\/)?([^\\#\\?\\/]*)?(/[^\\#\\?]*)?" uri)
    (declare (ignore slash))
    (let* ((uri-package-name (string-upcase (hunchentoot:url-decode firstlevel)))
	   (matching-prefix (find-if #'(lambda (package-uri)
					 (string-prefixp uri package-uri))
				     (cons (web-package-uri web-package)
					   (web-package-uri-aliases web-package)))))
    (cond 
      (matching-prefix
       (values matching-prefix secondlevel postfix))
      ((equal uri-package-name (symbol-name (web-package-name web-package)))
       (values (string-downcase uri-package-name) secondlevel postfix))
      (t (values nil))))))
    

(defmethod web-package-handles-request? ((web-package web-package) request)
  (let ((uri (hunchentoot:request-uri request)))
    (and (web-package-prefix-matches-uri? web-package uri) 
	 t)))

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

(defparameter *wiretap* *standard-output*)

(defmethod web-function-transform-request-into-arguments ((fn web-function) request)
  `(:postfix
    ,(multiple-value-bind (prefix fn-name-string postfix) 
	 (web-package-prefix-matches-uri? (web-package fn) (hunchentoot:request-uri request))
       (declare (ignore prefix fn-name-string))
       postfix)
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
    


(defmethod web-function-call-with-request ((fn web-function) request)
  (apply fn (web-function-transform-request-into-arguments fn request)))

(defmethod web-package-handler-for-request ((web-package web-package) request)
  (let ((uri (hunchentoot:request-uri request)))
    (multiple-value-bind (prefix fn-name-string postfix) 
	(web-package-prefix-matches-uri? web-package uri)
      (declare (ignore postfix))
      (when prefix
	(let* ((fn-name-string (and fn-name-string (string-upcase fn-name-string)))
	       (fn (if (and fn-name-string (< 0 (length fn-name-string)))
		       (find fn-name-string
			     (web-package-functions web-package)
			     :key #'(lambda (x) (symbol-name (web-function-name x)))
			     :test #'equal)
		       (find-web-function web-package (web-package-root-function-name web-package)))))
	  (if fn
	      #'(lambda ()
		  (let ((*web-package* web-package))
		    (web-function-call-with-request fn request)))
	      #'(lambda ()
		  (format nil "~A ~A"
			  fn-name-string
			  (web-package-functions web-package)))))))))

(defmethod web-package-handle-request ((web-package web-package) request)
  (let ((fn (web-package-handler-for-request web-package request)))
    (if fn
	(funcall fn)
	"nobody handled request!")))

(defun webfunk-hunchentoot-dispatcher (request)
  (or (loop :for web-package :in *web-packages*
	 :when (web-package-handles-request? web-package request)
	 :return #'(lambda () (web-package-handle-request web-package request)))
      #'(lambda () (format nil "Web packages: ~{ ~A ~}"
			   (mapcar #'(lambda (x)
				       (cons (web-package-uri x)
					     (web-package-handles-request? x request)))
				   *web-packages*)))))

(defun dispatch-web-functions (request)
  "This is a dispatcher which returns the appropriate handler defined with
WEB-DEFUN, if there is one."
  (webfunk-hunchentoot-dispatcher request))
;  (loop :for (uri server-names easy-handler) :in *easy-handler-alist*
;     :when (and (or (eq server-names t)
;		   (find (server-name *server*) server-names :test #'eq))
;	       (cond ((stringp uri)
;		      (string= (script-name request) uri))
;		     (t (funcall uri request))))
;     :do (return easy-handler)))

(defun make-keyword (string &key (destructivep nil))
  "Interns the upcased version of STRING into the KEYWORD package.
Uses NSTRING-UPCASE if DESTRUCTIVEP is true.  Returns NIL if STRING is
not a string."
  (and (stringp string)
       (intern (if destructivep
                 (nstring-upcase string)
                 (string-upcase string)) :keyword)))

(defun convert-parameter-string (param-string param-type)
  "Converts the string PARAM-STRING to PARAM-TYPE where PARAM-TYPE is one of the
symbols STRING, CHARACTERS, INTEGER, KEYWORD, or BOOLEAN - or
otherwise a function designator for a function of one argument.
ARGUMENT can also be NIL in which case this function also returns
NIL unconditionally."
  (when (listp param-string)
    ;; this if for the case that ARGUMENT is NIL or the result of a
    ;; file upload
    (return-from convert-parameter-string param-string))
  (case param-type
    (string param-string)
    (character (and (= (length param-string) 1)
                    (char param-string 0)))
    (number (ignore-errors (parse-number:parse-number param-string)))
    (real (ignore-errors (parse-number:parse-real-number param-string)))
    (integer (ignore-errors (parse-integer param-string :junk-allowed t)))
    (keyword (make-keyword param-string))
    (boolean t)
    (otherwise (error "Unsupported parameter conversion type: ~A" param-type))))

(defun compute-simple-parameter (parameter-name type parameter-reader)
  "Retrieves the parameter named PARAMETER-NAME using the reader
PARAMETER-READER and converts it to TYPE."
  (convert-parameter-string (funcall parameter-reader parameter-name) type))

(defun compute-list-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named
PARAMETER-NAME, converts them to TYPE, and returns a list of
them."
  (loop for (name . value) in parameters
        when (string= name parameter-name)
        collect (convert-parameter-string value type)))

(defun compute-array-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME[N]\" \(where N is a non-negative integer),
converts them to TYPE, and returns an array where the Nth element
is the corresponding value."
  ;; see <http://common-lisp.net/pipermail/tbnl-devel/2006-September/000660.html>
  #+:sbcl (declare (sb-ext:muffle-conditions warning))
  (let* ((index-value-list
          (loop for (full-name . value) in parameters
                for index = (cl-ppcre:register-groups-bind (name index-string)
                                ("^(.*)\\[(\\d+)\\]$" full-name)
                              (when (string= name parameter-name)
                                (parse-integer index-string)))
                when index
                collect (cons index (convert-parameter-string value type))))
         (array (make-array (1+ (reduce #'max index-value-list
                                        :key #'car
                                        :initial-value -1))
                            :initial-element nil)))
    (loop for (index . value) in index-value-list
          do (setf (aref array index) value))
    array))

(defun compute-hash-table-parameter (parameter-name type parameters key-type test-function)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME{FOO}\" \(where FOO is any sequence of characters
not containing curly brackets), converts them to TYPE, and
returns a hash table with test function TEST-FUNCTION where the
corresponding value is associated with the key FOO \(converted to
KEY-TYPE)."
  (let ((hash-table (make-hash-table :test test-function)))
    (loop for (full-name . value) in parameters
          for key = (cl-ppcre:register-groups-bind (name key-string)
                        ("^(.*){([^{}]+)}$" full-name)
                      (when (string= name parameter-name)
                        (convert-parameter-string key-string key-type)))
          when key
          do (setf (gethash key hash-table)
                   (convert-parameter-string value type)))
    hash-table))

(defun compute-parameter (parameter-name parameter-type request-type)
  "Computes and returns the parameter\(s) called PARAMETER-NAME
and converts it/them according to the value of PARAMETER-TYPE.
REQUEST-TYPE is one of :GET, :POST, or :BOTH."
  (when (member parameter-type '(list array hash-table))
    (setq parameter-type (list parameter-type 'string)))
  (let ((parameter-reader (ecase request-type
                              (:get #'hunchentoot:get-parameter)
                              (:post #'hunchentoot:post-parameter)
                              (:both #'hunchentoot:parameter)))
        (parameters (and (listp parameter-type)
                         (case request-type
                           (:get (hunchentoot:get-parameters))
                           (:post (hunchentoot:post-parameters))
                           (:both (append (hunchentoot:get-parameters) (hunchentoot:post-parameters)))))))
    (cond ((atom parameter-type)
           (compute-simple-parameter parameter-name parameter-type parameter-reader))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'list))
           (compute-list-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'array))
           (compute-array-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddddr parameter-type))
                (eq (first parameter-type) 'hash-table))
           (compute-hash-table-parameter parameter-name (second parameter-type) parameters
                                         (or (third parameter-type) 'string)
                                         (or (fourth parameter-type) 'equal)))
          (t (error "Don't know what to do with parameter type ~S." parameter-type)))))


;(defun make-function-var-instance-from-var-definition-list (list)
;  (if (atom list)
;      (make-instance 'web-function-var :name list

(defun href (fn-designator &optional (package *web-package*))
  (web-function-href fn-designator package))

(defun web-function-href (fn-designator &optional (package *web-package*))
  (let ((fn (if (functionp fn-designator) fn-designator (fdefinition fn-designator))))
    (format nil "/~A/~A"
	    (string-downcase (web-package-name package))
	    (string-downcase (web-function-name fn)))))

;(defun make-function-var-instance-from-var-definition-list (list)
;  (if (atom list)
;      (make-instance 'web-function-var :name list

(defun enough-url (url url-prefix)
  "Returns the relative portion of URL relative to URL-PREFIX, similar
to what ENOUGH-NAMESTRING does for pathnames."
  (subseq url (or (mismatch url url-prefix) (length url-prefix))))


(defun serve-static-file (given-path base-path &optional content-type)
  (declare (optimize debug))
  (let* ((given-path (subseq given-path 1))
	 (script-path (ppcre:regex-replace-all "\\\\" given-path "/"))
	 (script-path-directory (pathname-directory script-path)))
    (unless (or (stringp script-path-directory)
		(null script-path-directory)
		(and (listp script-path-directory)
		     (eq (first script-path-directory) :relative)
		     (loop for component in (rest script-path-directory)
                                    always (stringp component))))
      (setf (hunchentoot:return-code) hunchentoot:+http-forbidden+)
      (error "problem with ~S ~S" given-path script-path-directory)
      (throw 'hunchentoot::handler-done nil))
      
    (hunchentoot:handle-static-file (merge-pathnames script-path base-path) content-type)))
