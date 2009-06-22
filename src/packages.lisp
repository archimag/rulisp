;;; packages.lisp

(restas:define-plugin :rulisp-plugin
  (:use :cl :iter)
  (:basepath (asdf:component-pathname (asdf:find-system :rulisp))))


