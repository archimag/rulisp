;;; rulisp.asd

(defpackage :rulisp-system
  (:use :cl :asdf))

(in-package :rulisp-system)

(defsystem :rulisp
    :depends-on ( #:restas #:colorize #:planet #:postmodern #:ironclad #:cl-recaptcha #:wiki-parser #:zip #:cl-typesetting)
    :components
    ((:file "pref")
     (:module :src
              :components
              ((:file "packages")
               (:file "core" :depends-on ("packages"))
               (:file "rulisp" :depends-on ("core"))
               (:file "account" :depends-on ("core"))
               (:file "planet" :depends-on ("core"))
               (:module :forum
                        :components
                        ((:file "forum")
                         (:file "topics" :depends-on ("forum"))
                         (:file "messages" :depends-on ("forum"))
                         (:file "rss" :depends-on ("forum")))
                        :depends-on ("core"))
               (:file "apps" :depends-on ("core"))
               (:module :wiki
                        :components
                        ((:file "render-html")
                         (:file "render-pdf")
                         (:file "wiki" :depends-on ("render-html" "render-pdf")))
                        :depends-on ("core"))
               (:file "pcl"  :depends-on (:wiki))
               (:file "format" :depends-on ("core")))
              :depends-on ("pref"))
     (:file "start" :depends-on ("src"))))

