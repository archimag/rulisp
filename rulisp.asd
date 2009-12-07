;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
  :depends-on (#:restas #:colorize  #:postmodern #:ironclad #:cl-recaptcha
                        #:wiki-parser #:zip #:cl-libxslt #:xoverlay #:xfactory
                        #:restas-planet #:restas-wiki)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:file "utility" :depends-on ("packages"))               
                                     (:file "account" :depends-on ("utility"))
                                     (:file "static" :depends-on ("utility"))
                                     (:file "format" :depends-on ("static"))
                                     (:module :forum
                                              :components
                                              ((:file "forum")
                                               (:file "topics" :depends-on ("forum"))
                                               (:file "messages" :depends-on ("forum"))
                                               (:file "rss" :depends-on ("forum")))
                                              :depends-on ("static"))
                                     (:file "pcl"  :depends-on ("utility"))
                                     (:file "rulisp" :depends-on ("static" "account" :pcl :forum :format )))
                        :depends-on ("pref"))))

