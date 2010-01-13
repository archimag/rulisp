;;;; rulisp.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp)


(defun compute-user-login-name ()
  (restas:with-plugin-context (gethash 'rulisp-auth *site-plugins*)
    (restas.simple-auth::compute-user-login-name)))

(defparameter *mainmenu* `(("Главная" nil main)
                           ("Статьи" rulisp-articles restas.wiki:wiki-main-page)
                           ("Планета" rulisp-planet restas.planet:planet-main)
                           ("Форум" rulisp-forum rulisp.forum:forum-main)
                           ("Сервисы" nil tools-list)
                           ("Practical Common Lisp" rulisp-pcl rulisp.pcl:pcl-main)
                           ("Wiki" rulisp-wiki restas.wiki:wiki-main-page)
                           ("Файлы" rulisp-files restas.directory-publisher:route :path "")
                           ))

(defun css-files-data (files)
  (iter (for item in files)
        (collect (genurl 'css :theme (user-theme (username)) :file item))))

(defun toplevel-link-href (item)
  (apply  #'restas:site-url
          (gethash (second item) *site-plugins*)
          (if (cdddr item)
              (cddr item)
              (last item))))
  
(defun main-menu-data ()
  (iter (for item in *mainmenu*)
        (collect (list :href (toplevel-link-href item)
                       :name (first item)))))



(defclass rulisp-plugin-instance (restas:plugin-instance) ())

;;;; pcl

(restas:define-site-plugin rulisp-pcl (#:rulisp.pcl rulisp-plugin-instance)
  (rulisp.pcl:*baseurl* '("pcl")))

;;;; forum

(restas:define-site-plugin rulisp-forum (#:rulisp.forum rulisp-plugin-instance)
  (rulisp.forum:*baseurl* '("forum")))

;; ;;;; auth

(restas:define-site-plugin rulisp-auth (#:restas.simple-auth)
  (restas.simple-auth:*storage* *rulisp-db-storage*)
  (restas.simple-auth:*cookie-cipher-key* *cookie-cipher-key*)
  (restas.simple-auth:*finalize-page* (lambda (content)
                                     (rulisp.view.fine:main-frame (list :title (getf content :title)
                                                                        :css (css-files-data '("style.css"))
                                                                        :user (compute-user-login-name)
                                                                        :main-menu (main-menu-data)
                                                                        :content (getf content :body)
                                                                        :callback (hunchentoot:request-uri*))))))

;;;; format

(restas:define-site-plugin rulisp-format (#:restas.colorize)
  (restas.colorize:*baseurl* '("apps" "format"))
  (restas.colorize:*max-on-page* 15)
  (restas.colorize:*storage* *rulisp-db-storage*)
  (restas.colorize:*colorize-user-function* #'compute-user-login-name)  
  (restas.colorize:*finalize-page* (lambda (content)
                                     (rulisp.view.fine:main-frame (list :title (getf content :title)
                                                                        :css (css-files-data '("style.css" "colorize.css"))
                                                                        :user (compute-user-login-name)
                                                                        :main-menu (main-menu-data)
                                                                        :content (getf content :content)
                                                                        :callback (hunchentoot:request-uri*))))))

;;;; wiki

(restas:define-site-plugin rulisp-wiki (#:restas.wiki)
  (restas.wiki:*baseurl* '("wiki"))
  (restas.wiki:*wiki-dir* *wiki-dir*)  
  (restas.wiki:*wiki-user-function* #'compute-user-login-name)
  (restas.wiki:*finalize-page* #'(lambda (content)
                                   (rulisp.view.fine:main-frame (list :title (getf content :title)
                                                                      :css (css-files-data '("style.css" "wiki.css" "colorize.css"))
                                                                      :user (compute-user-login-name)
                                                                      :main-menu (main-menu-data)
                                                                      :content (getf content :content)
                                                                      :callback (hunchentoot:request-uri*))))))
;;;; articles

(restas:define-site-plugin rulisp-articles (#:restas.wiki)
  (restas.wiki:*baseurl* '("articles"))
  (restas.wiki:*index-page-title* "Статьи")
  (restas.wiki:*wiki-dir* #P"/var/rulisp/articles/")
  (restas.wiki:*wiki-user-function* #'(lambda ()
                                        (find (compute-user-login-name)
                                              '("archimag")
                                              :test #'string=)))
  (restas.wiki:*finalize-page* #'(lambda (content)
                                   (rulisp.view.fine:main-frame (list :title (getf content :title)
                                                                      :css (css-files-data '("style.css" "wiki.css" "colorize.css"))
                                                                      :user (compute-user-login-name)
                                                                      :main-menu (main-menu-data)
                                                                      :content (getf content :content)
                                                                      :callback (hunchentoot:request-uri*))))))
  


;;;; Russian Lisp Planet

(restas:define-site-plugin rulisp-planet (#:restas.planet)
  (restas.planet:*baseurl* '("planet"))
  (restas.planet:*suggest-mail* "archimag@lisper.ru")
  (restas.planet:*feeds* (merge-pathnames "planet-feeds.lisp" *rulisp-path*))
  (restas.planet:*name* "Russian Lisp Planet")  
  (restas.planet:*cache-dir* (merge-pathnames "planet/" *cachedir*))
  (restas.planet:*template* (lambda (data)
                              (rulisp.view.fine:main-frame (list :title "Russian Lisp Planet"
                                                                 :css (css-files-data '("style.css" "planet.css"))
                                                                 :user (compute-user-login-name)
                                                                 :main-menu (main-menu-data)
                                                                 :content (restas.planet.view:feed-html-body data)
                                                                 :callback (hunchentoot:request-uri*))))))

;;;; Files

(restas:define-site-plugin rulisp-files (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("files"))
  (restas.directory-publisher:*directory* (merge-pathnames "files/" *vardir*))
  (restas.directory-publisher:*autoindex-template*
   (lambda (data)
     (rulisp.view.fine:main-frame (list :title (getf data :title)
                                        :css (css-files-data '("style.css" "autoindex.css"))
                                        :user (compute-user-login-name)
                                        :main-menu (main-menu-data)
                                        :content (restas.directory-publisher.view:autoindex-content data)
                                        :callback (hunchentoot:request-uri*))))))
                                                                                        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chrome-resolver (url id ctxt)
  (declare (ignore id))
  (if (eql (puri:uri-scheme url) :chrome)
      (let* ((match-result (routes:match restas:*chrome-mapper*
                                         (concatenate 'string
                                                      (puri:uri-host url)
                                                      (puri:uri-path url))
                                         (acons :method :get (if (boundp 'restas:*bindings*)
                                                                 restas:*bindings*
                                                                 (restas:restas-request-bindings hunchentoot:*request*))))))
        (if match-result
            (gp:with-garbage-pool (restas:*request-pool*)
              (let ((restas:*bindings* (concatenate 'list (cdr match-result) restas:*bindings*)))
                (let ((result (restas:process-route (car match-result)
                                             (cdr match-result))))
                  (typecase result
                    (string (xtree:resolve-string result ctxt))
                    (pathname (xtree:resolve-file/url (namestring result) ctxt ))
                    (xtree::libxml2-cffi-object-wrapper (xtree:resolve-string (xtree:serialize result
                                                                                               :to-string)
                                                                              ctxt))))))))))



(defmethod restas:calculate-user-login ((instance rulisp-plugin-instance) request)
  (compute-user-login-name))

(defmethod restas:adopt-route-result ((instance rulisp-plugin-instance) (doc xtree:document))
  (if (string= (hunchentoot:content-type*) "text/html")
      (xtree:with-custom-resolvers (#'chrome-resolver)
        (xtree:with-object (res (xoverlay:apply-overlay (tmplpath "rulisp.html") doc :html t))
          (let ((str (xtree:serialize res :to-string :pretty-print t)))
            str)))
      (let ((str (xtree:serialize doc :to-string :pretty-print t)))
        str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(define-route mainmenu ("mainmenu"
                        :protocol :chrome)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :ul
        (iter (for item in *mainmenu*)
              (E :li
                 (E :a
                    (xfactory:attributes :href (toplevel-link-href item))
                    (xfactory:text (first item)))))))))


(define-route theme-css-include ("theme/css/:(file)"
                                        :protocol :chrome)
  (format nil
          "<link href=\"~A\" rel=\"stylesheet\" type=\"text/css\" />"
          (genurl 'css :theme (user-theme (username)) :file file)))
  

(defun rulisp-start ()
  (setf restas:*default-host-redirect*
        rulisp.preferences:*host*)
  (let ((hostname/port (restas:parse-host rulisp.preferences:*host*)))
    (restas:start-site :rulisp
                       :hostname (first hostname/port)
                       :port (second hostname/port))))
