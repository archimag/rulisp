;;;; rulisp.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp)


(defun compute-user-login-name ()
  (restas:with-submodule-context (gethash 'rulisp-auth *submodules*)
    (restas.simple-auth::compute-user-login-name)))


(defparameter *mainmenu* `(("Главная" nil main)
                           ("Статьи" rulisp-articles restas.wiki:wiki-main-page)
                           ("Планета" rulisp-planet restas.planet:planet-main)
                           ;;("Форум" rulisp-forum rulisp.forum:forum-main)
                           ("Форум" nil main)
                           ("Сервисы" nil tools-list)
                           ("Practical Common Lisp" rulisp-pcl rulisp.pcl:pcl-main)
                           ("Wiki" rulisp-wiki restas.wiki:wiki-main-page)
                           ("Файлы" rulisp-files restas.directory-publisher:route :path "")
                           ))

(defun css-files-data (files)
  (iter (for item in files)
        (collect (restas:genurl-toplevel nil
                                         'css
                                         :theme (user-theme (username))
                                         :file item))))


(defun gecko-png ()
  (restas:genurl-toplevel nil 'image :file "gecko.png"))

(defun toplevel-link-href (item)
  (apply  #'restas:genurl-toplevel
          (gethash (second item) *submodules*)
          (if (cdddr item)
              (cddr item)
              (last item))))
  
(defun main-menu-data ()
  (iter (for item in *mainmenu*)
        (collect (list :href (toplevel-link-href item)
                       :name (first item)))))

(defun rulisp-finalize-page (&key title css content)
  (rulisp.view.fine:main-frame (list :title title
                                     :css (css-files-data css)
                                     :gecko-png (restas:genurl-toplevel nil 'image :file "gecko.png")
                                     :user (compute-user-login-name)
                                     :main-menu (main-menu-data)
                                     :content content
                                     :callback (hunchentoot:request-uri*))))

;;;; pcl

(restas:define-submodule rulisp-pcl (#:rulisp.pcl)
  (rulisp.pcl:*baseurl* '("pcl")))

;; ;;;; auth

(restas:define-submodule rulisp-auth (#:restas.simple-auth)
  (restas.simple-auth:*storage* *rulisp-db-storage*)
  (restas.simple-auth:*noreply-email* *noreply-mail-account*)
  (restas.simple-auth:*cookie-cipher-key* *cookie-cipher-key*)
  (restas.simple-auth:*finalize-page* (lambda (content)
                                        (rulisp-finalize-page :title (getf content :title)
                                                              :css '("style.css")
                                                              :content (getf content :body)))))

;;;; format

(restas:define-submodule rulisp-format (#:restas.colorize)
  (restas.colorize:*baseurl* '("apps" "format"))
  (restas.colorize:*max-on-page* 15)
  (restas.colorize:*storage* *rulisp-db-storage*)
  (restas.colorize:*colorize-user-function* #'compute-user-login-name)  
  (restas.colorize:*finalize-page* (lambda (content)
                                     (rulisp-finalize-page :title (getf content :title)
                                                           :css '("style.css" "colorize.css")
                                                           :content (getf content :content)))))

;;;; wiki

(restas:define-submodule rulisp-wiki (#:restas.wiki)
  (restas.wiki:*baseurl* '("wiki"))
  (restas.wiki:*wiki-dir* *wiki-dir*)  
  (restas.wiki:*wiki-user-function* #'compute-user-login-name)
  (restas.wiki:*finalize-page* #'(lambda (content)
                                   (rulisp-finalize-page :title (getf content :title)
                                                         :css '("style.css" "wiki.css" "colorize.css")
                                                         :content (getf content :content)))))
;;;; articles

(restas:define-submodule rulisp-articles (#:restas.wiki)
  (restas.wiki:*baseurl* '("articles"))
  (restas.wiki:*index-page-title* "Статьи")
  (restas.wiki:*wiki-dir* #P"/var/rulisp/articles/")
  (restas.wiki:*wiki-user-function* #'(lambda ()
                                        (find (compute-user-login-name)
                                              '("archimag")
                                              :test #'string=)))
  (restas.wiki:*finalize-page* #'(lambda (content)
                                   (rulisp-finalize-page :title (getf content :title)
                                                         :css '("style.css" "wiki.css" "colorize.css")
                                                         :content (getf content :content)))))
  


;;;; Russian Lisp Planet

(restas:define-submodule rulisp-planet (#:restas.planet)
  (restas.planet:*baseurl* '("planet"))
  (restas.planet:*suggest-mail* "archimag@lisper.ru")
  (restas.planet:*feeds* (merge-pathnames "planet-feeds.lisp" *rulisp-path*))
  (restas.planet:*name* "Russian Lisp Planet")  
  (restas.planet:*cache-dir* (merge-pathnames "planet/" *cachedir*))
  (restas.planet:*template* (lambda (data)
                              (rulisp-finalize-page :title "Russian Lisp Planet"
                                                    :css '("style.css" "planet.css")
                                                    :content (restas.planet.view:feed-html-body data)))))
;;;; Files

(restas:define-submodule rulisp-files (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("files"))
  (restas.directory-publisher:*directory* (merge-pathnames "files/" *vardir*))
  (restas.directory-publisher:*autoindex-template*
   (lambda (data)
     (rulisp-finalize-page :title (getf data :title)
                           :css '("style.css" "autoindex.css")
                           :content (restas.directory-publisher.view:autoindex-content data)))))
                                                                                        

