;;; tools.lisp

(in-package :rulisp)

(define-route tools-list ("apps/"
                                 :overlay-master *master*)
  (tmplpath "apps.xml"))
                                 