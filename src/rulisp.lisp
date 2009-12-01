;;; rulisp.lisp

(in-package :rulisp)

(defparameter *mainmenu* `((rulisp-core main "Главная")
                           (rulisp-core articles "Статьи")
                           (rulisp-planet restas.planet:planet-main "Планета")
                           (rulisp-forum rulisp.forum:forum-main "Форум")
                           (rulisp-core tools-list "Сервисы")
                           (rulisp-pcl rulisp.pcl:pcl-main "Practical Common Lisp")
                           (rulisp-wiki rulisp.wiki:wiki-main-page "wiki")))

(defun css-files-data (files)
  (iter (for item in files)
        (collect (genurl 'css :theme (user-theme (username)) :file item))))
  
(defun main-menu-data ()
  (iter (for item in *mainmenu*)
        (collect (list :href (restas:site-url (gethash (first item) *site-plugins*)
                                              (second item))
                       :name (third item)))))



(defclass rulisp-plugin-instance (restas:plugin-instance) ())

;;;; core

(restas:define-site-plugin rulisp-core (:rulisp rulisp-plugin-instance))

;;;; wiki

(restas:define-site-plugin rulisp-wiki (:rulisp.wiki rulisp-plugin-instance)
  (rulisp.wiki:*baseurl* '("wiki")))

;;;; pcl

(restas:define-site-plugin rulisp-pcl (:rulisp.pcl rulisp-plugin-instance)
  (rulisp.pcl:*baseurl* '("pcl")))

;;;; forum

(restas:define-site-plugin rulisp-forum (:rulisp.forum rulisp-plugin-instance)
  (rulisp.forum:*baseurl* '("forum")))

;;;; format

(restas:define-site-plugin rulisp-format (:rulisp.format rulisp-plugin-instance)
  (rulisp.format:*baseurl* '("apps" "format")))

;;;; Russian Lisp Planet

(defclass rulisp-planet-plugin-instance (rulisp-plugin-instance) ())

(defmethod restas:adopt-route-result ((instance rulisp-planet-plugin-instance) (content string))
  (if (string= (hunchentoot:content-type*) "text/html")
      (rulisp.view.fine:main-frame (list :css (css-files-data '("style.css" "planet.css"))
                                         :user (username)
                                         :main-menu (main-menu-data)
                                         :content content))
      content))
                                                                      
(restas:define-site-plugin rulisp-planet (#:restas.planet rulisp-planet-plugin-instance)
  (restas.planet:*baseurl* '("planet"))
  (restas.planet:*suggest-mail* "archimag@lisper.ru")
  (restas.planet:*feeds* (merge-pathnames "planet-feeds.lisp" *rulisp-path*))
  (restas.planet:*name* "Russian Lisp Planet")
  (restas.planet:*template* 'restas.planet.view:feed-html-body)
  (restas.planet:*cache-dir* (merge-pathnames "planet/" *cachedir*)))

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
        (iter (for (plugin route name) in *mainmenu*)
              (E :li
                 (E :a
                    (xfactory:attributes :href
                                         (restas:site-url (gethash plugin *site-plugins*)
                                                          route))
                    (xfactory:text name))))))))


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
