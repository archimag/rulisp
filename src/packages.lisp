;;; packages.lisp

(restas:define-plugin :rulisp
  (:use :cl :iter :rulisp.preferences)
  (:basepath (asdf:component-pathname (asdf:find-system :rulisp))))
