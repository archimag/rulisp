;;;; static.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :rulisp)

(define-route main ("")
  (rulisp.view.fine:main-frame (list :title "Русскоязычное сообщество Common Lisp разработчиков"
                                     :css (css-files-data '("style.css"))
                                     :user (compute-user-login-name)
                                     :main-menu (main-menu-data)
                                     :gecko-png (gecko-png)
                                     :content (alexandria:read-file-into-string (merge-pathnames "content/index.xml"
                                                                                                 *rulisp-path*))
                                     :callback (hunchentoot:request-uri*))))


(define-route css ("/css/:(theme)/:(file)")
  (skinpath (format nil "css/~A" file)
            theme))

(define-route image ("image/:(file)")
  (staticpath (format nil "image/~A" file)))

(define-route js ("js/:(file)")
  (staticpath (format nil "js/~A" file)))

(define-route favicon ("favicon.ico")
  (staticpath "favicon.ico"))

(define-route tools-list ("apps/")
  (rulisp.view.fine:main-frame (list :title "Инструменты"
                                     :css (css-files-data '("style.css"))
                                     :user (compute-user-login-name)
                                     :main-menu (main-menu-data)
                                     :content (alexandria:read-file-into-string (tmplpath "apps.xml"))
                                     :gecko-png (gecko-png)
                                     :callback (hunchentoot:request-uri*))))

