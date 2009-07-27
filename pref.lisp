;;; pref.lisp

(defpackage :rulisp.preferences
  (:use :cl)
  (:export :*rulisp-path*
           :*skindir*
           :*rulisp-db*
           :*host*))

(in-package :rulisp.preferences)

(defparameter *rulisp-path* (asdf:component-pathname (asdf:find-system  :rulisp)))

(defparameter *skindir* (merge-pathnames "skins/default/" *rulisp-path*))

(defparameter *rulisp-db* '("rulisp" "lisp" "123" "localhost"))

(defparameter *host* "lisp.catap.ru")
