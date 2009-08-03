;;; pref.lisp

(defpackage :rulisp.preferences
  (:use :cl)
  (:export :*rulisp-path*
           :*skindir*
           :*rulisp-db*
           :*host*
           :*cookie-cipher-key*
           :*noreply-mail-account*
           :*vardir*
           :*reCAPTCHA.publick-key*
           :*reCAPTCHA.privake-key*))

(in-package :rulisp.preferences)

(defparameter *vardir* #P"/var/rulisp/")

(defparameter *rulisp-path* (asdf:component-pathname (asdf:find-system  :rulisp)))

(defparameter *skindir* (merge-pathnames "skins/" *rulisp-path*))

(defparameter *default-theme* "archimag")

(defparameter *rulisp-db* '("rulisp" "lisp" "123" "localhost"))

(defparameter *host* "localhost:8080")

(defparameter *cookie-cipher-key* (ironclad:ascii-string-to-byte-array "Specify the secure key"))

(defparameter *noreply-mail-account* "noreply@lisper.ru")

(defvar *reCAPTCHA.publick-key* "6LdZjAcAAAAAAGh_MzHcHfJWp6rpI0XUNghGQB1f")
(defvar *reCAPTCHA.privake-key* "6LdZjAcAAAAAAKJ2GPWTHPh1H1Foc0kyfbwgrFgO")
