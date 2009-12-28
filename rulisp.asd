;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
  :depends-on (#:restas #:postmodern #:ironclad 
                        #:cl-recaptcha
                        #:zip #:cl-libxslt #:xoverlay #:xfactory
                        #:restas-planet #:restas-wiki #:restas-colorize #:restas-directory-publisher)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:file "utility" :depends-on ("packages"))               
                                     (:file "account" :depends-on ("utility"))
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
                                     (:file "rulisp" :depends-on ("static" "account" "storage" :pcl :forum)))
                        :depends-on ("pref"))))

