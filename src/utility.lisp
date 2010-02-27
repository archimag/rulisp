;;;; core.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :rulisp)

(defparameter *basepath* (asdf:component-pathname (asdf:find-system :rulisp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *static-path* (merge-pathnames "static/" *basepath*))

(defun staticpath (path)
  (merge-pathnames path *static-path*))

(postmodern:defprepared user-theme* "SELECT theme FROM users WHERE login = $1" :single)

(defmacro with-rulisp-db (&body body)
  `(postmodern:with-connection *rulisp-db*
     ,@body))

(defun user-theme (name)
  (let ((theme (if name
                   (with-rulisp-db (user-theme* name)))))
    (if (and theme
             (not (eql theme :null))
             (fad:directory-exists-p (merge-pathnames theme *skindir*)))
        theme
        *default-skin*)))

(defun skinpath (path &optional theme)
  (let ((result (merge-pathnames path
                                 (format nil "~A/~A/"  *skindir* (or theme
                                                                     (user-theme (username)))))))
    (if (fad:file-exists-p result)
        result
        (merge-pathnames path
                         (format nil "~A/default/"  *skindir*)))))

(defun tmplpath (path)
  (skinpath (merge-pathnames path "templates/")))

