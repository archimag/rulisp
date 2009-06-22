;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
    :depends-on ( #:restas-new #:colorize)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "rulisp" :depends-on ("packages"))
               (:file "start" :depends-on ("rulisp"))))))
