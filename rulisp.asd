;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
  :depends-on (#:restas #:colorize #:planet #:postmodern #:ironclad #:cl-recaptcha
                        #:wiki-parser #:zip #:cl-libxslt :xoverlay #:cl-typesetting)
    :components
    ((:file "pref")
     (:module :src
              :components
              ((:file "packages")
               (:file "utility" :depends-on ("packages"))               
               (:file "account" :depends-on ("utility"))
               (:file "format" :depends-on ("utility"))
               (:file "planet" :depends-on ("utility"))
               (:file "static" :depends-on ("utility"))
               (:module :forum
                        :components
                        ((:file "forum")
                         (:file "topics" :depends-on ("forum"))
                         (:file "messages" :depends-on ("forum"))
                         (:file "rss" :depends-on ("forum")))
                        :depends-on ("utility"))
;;                (:file "apps" :depends-on ("utility"))
               (:module :wiki
                        :components
                        ((:file "render-html")
                         (:file "render-pdf")
                        (:file "wiki" :depends-on ("render-html" "render-pdf")))
                        :depends-on ("utility"))
               (:file "pcl"  :depends-on ("wiki"))
               (:file "rulisp" :depends-on ("static" "account" :pcl :forum :format :planet))
               )
              :depends-on ("pref"))
     (:file "start" :depends-on ("src"))))

