(defpackage :webfunk-scripts
    (:use :common-lisp)
  (:export #:generate-project))

(in-package :webfunk-scripts)


(defun slurp-file-3000 (pathname)
  "A SLURP-FILE function inspired Mr. Insane 3000's
     SLURP-STREAM4."
  (with-open-file (strm pathname)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))

(defparameter *template-env* nil)

(defun file-contents (system-relative-pathname)
  "Returns the contents of a file at a path relative to the current system."
  (slurp-file-3000
   (asdf:system-relative-pathname (asdf:find-system :webfunk)
				  system-relative-pathname)))

(cl-emb:register-emb "lisp-daemon-template" (file-contents "script/templates/lisp-daemon.template"))
(cl-emb:register-emb "start-daemon-template" (file-contents "script/templates/start-daemon.template"))
(cl-emb:register-emb "pages-template" (file-contents "script/templates/pages.template"))
(cl-emb:register-emb "server-template" (file-contents "script/templates/server.template"))
(cl-emb:register-emb "package-template" (file-contents "script/templates/package.template"))
(cl-emb:register-emb "asd-template" (file-contents "script/templates/webfunk-asd.template"))
(cl-emb:register-emb "paren-site-template" (file-contents "script/templates/site.paren.template"))

(defun output-file-from-template (out-pathname template-name &key if-exists)
  (with-open-file (stream out-pathname :direction :output :if-exists if-exists)
    (write-string (cl-emb:execute-emb  template-name :env *template-env*) stream)))
  
(defun generate-project (&key output-directory package-name author package-title
			 if-exists)
  (let* ((src-dir (merge-pathnames "src/" output-directory))
	 (paren-dir (merge-pathnames "paren/" src-dir)))
    (flet ((ensure-dir (dir)
	     (format t "Ensuring directory ~A exists (or creating it if it does not)...~%" dir)
	     (ensure-directories-exist dir :verbose t)))

      (ensure-dir output-directory)
      (ensure-dir src-dir)
      (ensure-dir paren-dir))

    (format t "Generating files from templates...")

    (let* ((package-name (string-downcase (string package-name)))
	   (system-package-name (format nil "~A.script" package-name))
	   (asd-pathname (merge-pathnames (make-pathname :name package-name :type "asd")
					  output-directory))
	   (server-pathname (merge-pathnames (make-pathname :name "server" :type "lisp") src-dir))
	   (pages-pathname (merge-pathnames (make-pathname :name "pages" :type "lisp") src-dir))
	   (startd-pathname (merge-pathnames (make-pathname :name "start-daemon" :type "lisp") output-directory))
	   (lispd-pathname (merge-pathnames (make-pathname :name "lisp-daemon" :type nil) output-directory))
	   (package-pathname (merge-pathnames (make-pathname :name "package" :type "lisp") src-dir))
	   (paren-site-pathname (merge-pathnames (make-pathname :name "site" :type "paren") paren-dir))
	   (*template-env* `(:package-name ,package-name
					   :output-directory ,output-directory
					   :system-package-name ,system-package-name
					   :author ,author
					   :package-title ,package-title)))

      (output-file-from-template asd-pathname "asd-template" :if-exists if-exists)
      (output-file-from-template server-pathname "server-template"  :if-exists if-exists)
      (output-file-from-template pages-pathname "pages-template"  :if-exists if-exists)
      (output-file-from-template startd-pathname "start-daemon-template" :if-exists if-exists)
      (output-file-from-template lispd-pathname "lisp-daemon-template" :if-exists if-exists)
      (output-file-from-template package-pathname "package-template" :if-exists if-exists)
      (output-file-from-template paren-site-pathname "paren-site-template" :if-exists if-exists)
      
      (format t "Done!")
      (values))))