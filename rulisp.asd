;;;; rulisp.asd
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem rulisp
  :depends-on (#:restas
               #:simple-date #:postmodern
               #:zip
               #:restas-simple-auth #:restas-planet #:restas-wiki
               #:restas-colorize #:restas-directory-publisher #:restas-forum
               #:xfactory #:cl-typesetting
               #:wiki-parser)
  :components ((:file "pref")
               (:module :src
                        :components ((:file "packages")
                                     (:module "dokuwiki"
                                              :components ((:file "render-pdf")
                                                           (:file "render-html"))
                                              :depends-on ("packages"))
                                     (:file "storage" :depends-on ("packages"))
                                     (:file "pcl"  :depends-on ("rulisp"))
                                     (:file "jscl"  :depends-on ("rulisp"))
                                     (:file "rulisp" :depends-on ("storage" "dokuwiki")))
                        :depends-on ("pref"))))

