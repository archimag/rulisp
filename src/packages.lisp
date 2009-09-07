;;; packages.lisp

;; (restas:define-plugin :rulisp
;;   (:use :cl :iter :rulisp.preferences)
;;   (:basepath (asdf:component-pathname (asdf:find-system :rulisp))))

(defpackage :rulisp
  (:use :cl :iter :rulisp.preferences)
  (:export #:code-to-html
           #:calc-md5-sum
           #:calc-sha1-sum
           #:write-string-into-gzip-file
           #:read-gzip-file-into-string
           #:substring
           #:username
           #:expand-file
           #:in-pool
           #:with-rulisp-db
           #:*re-email-check*
           #:redirect
           #:genurl-with-host
           #:eid
           #:eclass
           #:ehref
           #:estyle
           #:escript
           #:ecss
           #:e-break-line
           #:estrong
           #:e-text2html
           #:etext
           #:send-mail
           #:send-noreply-mail
           #:form-error-message
           #:form-field-value
           #:form-field-empty-p
           #:fill-form
           #:staticpath
           #:user-theme
           #:skinpath
           #:tmplpath
           #:*rulisp-path*
           ))