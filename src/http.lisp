(in-package :webfunk)

#+nil ;; i felt like this should be left to the user to track etc.
(defvar *webfunk-server* nil
  "Bound to the active webfunk server.  There may be multiple webfunk servers in the same running
lisp, but this variable will only be bound to one of them.")


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter *catch-errors-p* t
    "True if WebFunk should catch errors.  False if webfunk should let them rise up."))

;;; An acceptor that invokes the debugger on errors:                                                                                                                                                           
(defclass webfunk-acceptor (hunchentoot:acceptor)
    ())

(defmacro ignoring-network-errors (&body body)
  (let ((blockname (gensym "network-error-block")))
    `(block ,blockname
       (handler-bind
	   ;; ignore connection errors
	   (#+sbcl 
	    (sb-sys:io-timeout #'(lambda (e) (declare (ignore e)) (return-from ,blockname nil)))
	    (usocket:timeout-error #'(lambda (e) (declare (ignore e)) (return-from ,blockname nil)))
	    #+sbcl
	    (sb-int:simple-stream-error
	     #'(lambda (e) (declare (ignore e))  (return-from ,blockname nil))))
	 ,@body))))

(defmethod hunchentoot:maybe-invoke-debugger ((condition usocket:timeout-error))
   "Ignored timeout-errors as they are the norm."
   nil)

(defmacro maybe-ignoring-errors (&body body)
  (let ((blockname (gensym "maybe-ignore-errors-block")))
    `(block ,blockname
       (handler-bind
	   ((error #'(lambda (e)
		      (if *catch-errors-p*
			  (return-from ,blockname nil)
			  (invoke-debugger e)))))
	 (ignoring-network-errors ,@body)))))
       
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun webfunk-declarations ()
    '(declare (optimize (debug 3)))))

(defmethod hunchentoot:process-connection ((hunchentoot:*acceptor* webfunk-acceptor) (socket t))
  (declare (ignore socket))
  #.(webfunk-declarations)
  (maybe-ignoring-errors (call-next-method)))

(defmethod hunchentoot:acceptor-request-dispatcher ((hunchentoot:*acceptor* webfunk-acceptor))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (maybe-ignoring-errors
	(funcall dispatcher request)))))
        
(defun webfunk-request-dispatcher (request)
  (let* ((dispatch-table (list 'webfunk-hunchentoot-dispatcher)))
    (loop :for dispatcher :in dispatch-table
	  :for handler = (funcall dispatcher request)
	  :when handler
	  :return (funcall handler))))

(defun start-http-server (&key (port 80))
  "Starts up a web server."
  (declare (type integer port))
  (let ((server (make-instance 'webfunk-acceptor
			       :port port
			       :request-dispatcher 'webfunk-request-dispatcher)))
    (hunchentoot:start server)
    server))

(defun stop-http-server (server)
  "Stops a web server."
  (when server
    (hunchentoot:stop server)))
