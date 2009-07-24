;;; packages.lisp

(restas:define-plugin :rulisp
  (:use :cl :iter)
  (:basepath (asdf:component-pathname (asdf:find-system :rulisp))))
