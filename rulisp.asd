;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
    :depends-on ( #:restas #:colorize #:planet #:postmodern #:ironclad)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "core" :depends-on ("packages"))
               (:file "rulisp" :depends-on ("core"))
               (:file "account" :depends-on ("core"))
               (:file "planet" :depends-on ("core"))
               (:file "forum" :depends-on ("core"))
               (:file "start" :depends-on ("rulisp" "planet" "forum"))))))
