;;; packages.lisp

(restas:defsite :rulisp
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences)
  (:export #:code-to-html           
           #:substring
           #:username

           #:with-rulisp-db
           #:*re-email-check*

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

           #:rulisp-start
           ))

(restas:define-plugin #:rulisp.forum
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences))

;; (restas:define-plugin #:rulisp.wiki
;;   (:use #:cl #:iter #:restas.optional #:rulisp.preferences))

(restas:define-plugin :rulisp.pcl
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences))

(restas:define-plugin :rulisp.planet
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences))

(restas:define-plugin :rulisp.format
  (:use :cl :iter #:restas.optional))


(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "static/skins/fine/templates/rulisp.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:rulisp))))