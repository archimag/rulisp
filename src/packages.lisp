;;;; packages.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:rulisp
  (:use #:cl #:iter #:rulisp.preferences)
  (:export #:substring
           #:username

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
  (:use #:cl #:iter #:rulisp.preferences)
  (:export #:pcl-main))

