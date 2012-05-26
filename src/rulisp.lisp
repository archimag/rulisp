;;;; rulisp.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compute login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-user-login-name ()
  (restas:with-submodule (restas:find-submodule 'rulisp-auth
                                                (restas:find-upper-submodule #.*package*))
    (restas.simple-auth::compute-user-login-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rulisp templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *resources-dir*
    (merge-pathnames "resources/"
                     (asdf:component-pathname (asdf:find-system '#:rulisp))))

  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "rulisp.tmpl"
                                                      *resources-dir*)))

(defparameter *mainmenu* `(("Главная" nil main)
                           ("Статьи" rulisp-articles restas.wiki:main-wiki-page)
                           ("Планета" rulisp-planet restas.planet:planet-main)
                           ("Форум" rulisp-forum restas.forum:list-forums)
                           ("Сервисы" nil tools-list)
                           ("Practical Common Lisp" rulisp-pcl rulisp.pcl:pcl-main)
                           ("Wiki" rulisp-wiki restas.wiki:main-wiki-page)
                           ("Файлы" rulisp-files restas.directory-publisher:route :path "")
                           ("Поиск" nil google-search)))

(defun rulisp-finalize-page (&key title css js content)
  (rulisp.view:main-frame 
   (list :title title
         :css (iter (for item in css)
                    (collect (format nil "/css/~A" item)))
         :js js
         :gecko-png  "/image/gecko.png"
         :user (compute-user-login-name)
         :main-menu (restas:with-submodule (restas:find-upper-submodule #.*package*)
                      (iter (for item in *mainmenu*)
                            (collect (list :href (apply #'restas:genurl-submodule
                                                        (second item)
                                                        (if (cdddr item)
                                                            (cddr item)
                                                            (last item)))
                                           :name (first item)))))
         :content content
         :callback (hunchentoot:request-uri*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route main ("")
  (rulisp-finalize-page :title "Русскоязычное сообщество Common Lisp разработчиков"
                        :css '("style.css")
                        :content (alexandria:read-file-into-string (merge-pathnames "index.html"
                                                                                    *resources-dir*))))

(restas:define-route tools-list ("apps/")
  (rulisp-finalize-page :title "Инструменты"
                        :css '("style.css")
                        :content (rulisp.view:tools)))

(restas:define-route google-search ("search")
  (rulisp-finalize-page :title "Поиск по сайту Lisper.ru"
                        :css '("style.css")
                        :content (rulisp.view:google-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; submodules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rulisp-drawer () ())

(defmethod restas:render-object ((designer rulisp-drawer) (code (eql hunchentoot:+http-not-found+)))
  (rulisp-finalize-page :title "Not Found"
                        :css '("style.css")
                        :content (rulisp.view:not-found-content (list :href (hunchentoot:request-uri*)))))
;;;; pcl

(restas:mount-submodule rulisp-pcl (#:rulisp.pcl)
  (rulisp.pcl:*baseurl* '("pcl")))

;; ;;;; auth

(restas:mount-submodule rulisp-auth (#:restas.simple-auth)
  (restas.simple-auth:*storage* *rulisp-db-storage*)
  (restas.simple-auth:*noreply-email* *noreply-mail-account*)
  (restas.simple-auth:*cookie-cipher-key* *cookie-cipher-key*)
  (restas.simple-auth:*finalize-page* (lambda (content)
                                        (rulisp-finalize-page :title (getf content :title)
                                                              :css '("style.css")
                                                              :content (getf content :body)))))

;;;; forum

(restas:mount-submodule rulisp-forum (#:restas.forum)
  (restas.forum:*baseurl* '("forum"))
  (restas.forum:*site-name* "Lisper.ru")
  (restas.forum:*storage* *rulisp-db-storage*)
  (restas.forum:*user-name-function* #'compute-user-login-name)
  (restas.forum:*default-render-method*
   (lambda (obj)
     (rulisp-finalize-page :title (getf obj :title)
                           :content (restas:render-object (find-package '#:restas.forum.view) 
                                                          obj)
                           :css '("style.css" "jquery.wysiwyg.css" "forum.css" "colorize.css" )
                           :js (getf obj :js)))))

;;;; format

(defclass pastebin-drawer (restas.colorize::drawer) ())

(defmethod restas.colorize::finalize-page ((drawer pastebin-drawer) data)
  (rulisp-finalize-page  :title (getf data :title)
                         :css '("style.css" "colorize.css")
                         :content (concatenate 'string
                                               (getf data :menu)
                                               (getf data :content))))


(restas:mount-submodule rulisp-format (#:restas.colorize)
  (restas.colorize:*baseurl* '("apps" "format"))
  (restas.colorize:*max-on-page* 15)
  (restas.colorize:*storage* *rulisp-db-storage*)
  (restas.colorize:*colorize-user-function* #'compute-user-login-name)
  (restas.colorize:*default-render-method* (make-instance 'pastebin-drawer)))

;;;; wiki

(defclass drawer (dokuwiki-drawer) ())

(defmethod restas.wiki:finalize-page ((drawer drawer) content)
  (rulisp-finalize-page :title (getf content :title)
                        :css '("style.css" "wiki.css" "colorize.css")
                        :content (concatenate 'string
                                              (restas.wiki.view:show-page-menu (getf content :menu-links))
                                              (getf content :content))))


(restas:mount-submodule rulisp-wiki (#:restas.wiki)
  (restas.wiki:*baseurl* '("wiki"))
  (restas.wiki:*storage* (make-instance 'restas.wiki:file-storage :dir *wiki-dir*))
  (restas.wiki:*wiki-user-function* #'compute-user-login-name)
  (restas.wiki:*default-render-method* (make-instance 'drawer)))

;;;; articles

(restas:mount-submodule rulisp-articles (#:restas.wiki)
  (restas.wiki:*baseurl* '("articles"))
  (restas.wiki:*index-page-title* "Статьи")
  (restas.wiki:*storage* (make-instance 'restas.wiki:file-storage
                                        :dir #P"/var/rulisp/articles/"))
  (restas.wiki:*wiki-user-function* #'(lambda ()
                                        (find (compute-user-login-name)
                                              '("archimag" "dmitry_vk")
                                              :test #'string=)))
  (restas.wiki:*default-render-method* (make-instance 'drawer)))
  


;;;; Russian Lisp Planet

(restas:mount-submodule rulisp-planet (#:restas.planet)
  (restas.planet:*baseurl* '("planet"))
  (restas.planet:*suggest-mail* "archimag@lisper.ru")
  (restas.planet:*feeds* (merge-pathnames "planet-feeds.lisp" *rulisp-path*))
  (restas.planet:*name* "Russian Lisp Planet")  
  (restas.planet:*cache-dir* (merge-pathnames "planet/" *cachedir*))
  (restas.planet:*template* (lambda (data)
                              (rulisp-finalize-page :title "Russian Lisp Planet"
                                                    :css '("style.css" "planet.css")
                                                    :content (restas.planet.view:feed-html-body data)))))

;;;; static files

(closure-template:ensure-ttable-package
 '#:rulisp.directory-publisher.view
 :prototype (closure-template:package-ttable '#:restas.directory-publisher.view))

(let ((ttable (closure-template:package-ttable '#:restas.directory-publisher.view)))
  (flet ((rulisp-autoindex (data out)
           (write-string (rulisp-finalize-page :title (getf data :title)
                                               :css '("style.css" "autoindex.css")
                                               :content (restas.directory-publisher.view:autoindex-content data))
                         out)))
    (closure-template:ttable-register-template ttable "AUTOINDEX" #'rulisp-autoindex :supersede t)))

(defclass static-autoindex-view (rulisp-drawer restas.directory-publisher:view) ()
  (:default-initargs
   :template-package '#:rulisp.directory-publisher.view))

(defclass rulisp-static-view (rulisp-drawer restas.directory-publisher:view) ())

(restas:mount-submodule rulisp-static (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "static/" *resources-dir*))
  (restas.directory-publisher:*default-render-method* (make-instance 'static-autoindex-view)))

(restas:mount-submodule rulisp-files (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("files"))
  (restas.directory-publisher:*directory* (merge-pathnames "files/" *vardir*))
  (restas.directory-publisher:*default-render-method* (make-instance 'static-autoindex-view)))
                                                                                        

