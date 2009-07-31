;;; tools.lisp

(in-package :rulisp)

(define-simple-route tools-list ("apps/"
                                 :overlay-master *master*)
  (skinpath "apps.xml"))
                                 