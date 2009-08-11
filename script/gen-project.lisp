(asdf:operate 'asdf:load-op :cl-emb)

(defun slurp-file-3000 (pathname)
  "A SLURP-FILE function inspired Mr. Insane 3000's
     SLURP-STREAM4."
  (with-open-file (strm pathname)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))

(defparameter *template-env* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-emb:register-emb "main-template" (slurp-file-3000 "script/templates/main.template"))
  (cl-emb:register-emb "package-template" (slurp-file-3000 "script/templates/package.template"))
  (cl-emb:register-emb "asd-template" (slurp-file-3000 "script/templates/webfunk-asd.template"))
  (cl-emb:register-emb "paren-site-template" (slurp-file-3000 "script/templates/site.paren.template")))

(defun output-file-from-template (out-pathname template-name)
  (with-open-file (stream out-pathname :direction :output)
    (write-string (cl-emb:execute-emb  template-name :env *template-env*) stream)))
  
(defun generate-project (&key output-directory package-name author package-title)
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
	   (main-pathname (merge-pathnames (make-pathname :name "pages" :type "lisp") src-dir))
	   (package-pathname (merge-pathnames (make-pathname :name "package" :type "lisp") src-dir))
	   (paren-site-pathname (merge-pathnames (make-pathname :name "site" :type "paren") paren-dir))
	   (*template-env* `(:package-name ,package-name
					   :system-package-name ,system-package-name
					   :author ,author
					   :package-title ,package-title)))

      (output-file-from-template asd-pathname "asd-template")
      (output-file-from-template main-pathname "main-template")
      (output-file-from-template package-pathname "package-template")
      (output-file-from-template paren-site-pathname "paren-site-template")
      
      (format t "Done!")
      (values))))