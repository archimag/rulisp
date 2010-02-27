;;;; packages.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:defsite #:rulisp
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences)
  (:export #:substring
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

(restas:define-module #:rulisp.pcl
  (:use #:cl #:iter #:restas.optional #:rulisp.preferences))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "static/skins/fine/templates/rulisp.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:rulisp))))