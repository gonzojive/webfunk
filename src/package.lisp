(defpackage :webfunk
    (:nicknames :wf)
  (:use :common-lisp :anaphora :alexandria)
  (:export 
   ;; macros
   #:web-defun
   ;; web packages
   #:web-defpackage #:webpackage
   #:*web-packages*
   #:*web-package*
   #:*root-web-package*
   #:web-package
   #:find-web-package
   #:in-web-package
   #:web-package-name
   #:web-package-functions
   #:web-package-uri
   #:web-package-uri-aliases
   ;; web functions
   #:web-function

   #:remove-web-function
   #:delete-web-package

   #:web-function-href
   #:href

   #:called-from-web?

   ;; serving files
   #:serve-static-file
   
   ;; hunchentoot plug-in
   #:webfunk-hunchentoot-dispatcher
   #:*catch-errors-p*
   #:start-http-server
   #:stop-http-server
   #:webfunk-acceptor
   
   ;; types
;   #:json-encoded))
   ))

(in-package :webfunk)
