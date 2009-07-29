;;; paste.lisp

(in-package :rulisp)


(define-filesystem-route paste-main "paste/" (skinpath "paste-form.xml")
                         :overlay-master *master*
                         :login-status :logged-on)

(define-filesystem-route paste-main "paste/" (skinpath "paste-form-not-logged.xml")
                         :overlay-master *master*
                         :login-status :not-logged-on)