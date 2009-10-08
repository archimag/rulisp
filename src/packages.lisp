;;; packages.lisp

;; (restas:define-plugin :rulisp
;;   (:use :cl :iter :rulisp.preferences)
;;   (:basepath (asdf:component-pathname (asdf:find-system :rulisp))))

(restas:define-plugin :rulisp
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences)
  (:export #:code-to-html
           #:calc-md5-sum
           #:calc-sha1-sum
           #:write-string-into-gzip-file
           #:read-gzip-file-into-string
           #:substring
           #:username
           #:in-pool
           #:with-rulisp-db
           #:*re-email-check*
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

           #:image
           ))

(restas:define-plugin #:rulisp.forum
  (:use #:cl #:iter #:restas.optional #:rulisp #:rulisp.preferences))

(restas:define-plugin #:rulisp.wiki
  (:use #:cl #:iter #:restas.optional #:rulisp #:rulisp.preferences))

(restas:define-plugin :rulisp.pcl
  (:use #:cl #:iter #:restas.optional #:rulisp #:rulisp.preferences))

(restas:define-plugin :rulisp.planet
  (:use #:cl #:iter #:rulisp #:rulisp.preferences))
