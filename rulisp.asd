;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
    :depends-on ( #:restas #:colorize #:planet)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "rulisp" :depends-on ("packages"))
               (:file "planet" :depends-on ("rulisp"))
               (:file "start" :depends-on ("rulisp"))))))
