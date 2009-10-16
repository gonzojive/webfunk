(defpackage :webfunk-tests
    (:use :common-lisp :webfunk :stefil)
  (:nicknames :wf-tests)
  (:export #:webfunk-tests #:run-tests))

(in-package :webfunk-tests)

(defsuite webfunk-tests)

(in-suite webfunk-tests)

(defun run-tests ()
  (webfunk-tests))