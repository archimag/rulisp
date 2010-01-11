;;;; rulisp.asd
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem rulisp
  :depends-on (#:restas #:postmodern #:ironclad 
                        #:cl-recaptcha
                        #:zip
                        #:cl-libxslt #:xoverlay #:xfactory
                        #:restas-simple-auth #:restas-planet #:restas-wiki
                        #:restas-colorize #:restas-directory-publisher)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:file "utility" :depends-on ("packages"))               
                                     ;;(:file "account" :depends-on ("utility"))
                                     (:file "static" :depends-on ("utility"))
                                     (:file "storage" :depends-on ("packages"))
                                     (:module :forum
                                              :components
                                              ((:file "forum")
                                               (:file "topics" :depends-on ("forum"))
                                               (:file "messages" :depends-on ("forum"))
                                               (:file "rss" :depends-on ("forum")))
                                              :depends-on ("static"))
                                     (:file "pcl"  :depends-on ("utility"))
                                     (:file "rulisp" :depends-on ("static" "storage" :pcl :forum)))
                        :depends-on ("pref"))))

