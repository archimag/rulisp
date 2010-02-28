;;;; rulisp.asd
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem rulisp
  :depends-on (#:restas 
               #:postmodern
               #:cl-recaptcha
               #:zip
               #:restas-simple-auth #:restas-planet #:restas-wiki
               #:restas-colorize #:restas-directory-publisher)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:file "storage" :depends-on ("packages"))
                                     (:file "pcl"  :depends-on ("rulisp"))
                                     (:file "rulisp" :depends-on ("storage")))
                        :depends-on ("pref"))))

