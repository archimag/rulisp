;;; test.lisp

(in-package :rulisp)

(restas::defsite)

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

(defclass rulisp-plugin-instance (restas:plugin-instance) ())

(defmethod restas:calculate-user-login ((instance rulisp-plugin-instance) request)
  (rulisp.account::compute-user-login-name))

(defmethod restas:adopt-route-result ((instance rulisp-plugin-instance) (doc xtree:document))
  (if (string= (hunchentoot:content-type*) "text/html")
      (xtree:with-custom-resolvers (#'chrome-resolver)
        (xtree:with-object (res (xoverlay:apply-overlay (tmplpath "rulisp.html") doc :html t))
          (let ((str (xtree:serialize res :to-string :pretty-print t)))
            str)))
      (let ((str (xtree:serialize doc :to-string :pretty-print t)))
        str)))


(restas::define-site-plugin rulisp-static (:rulisp.static 'rulisp-plugin-instance))
(restas::define-site-plugin rulisp-account (:rulisp.account 'rulisp-plugin-instance))

(restas::define-site-plugin rulisp-wiki (:rulisp.wiki 'rulisp-plugin-instance)
  (rulisp.wiki::*baseurl* ("wiki")))

(restas::define-site-plugin rulisp-pcl (:rulisp.pcl 'rulisp-plugin-instance)
  (rulisp.pcl::*baseurl* ("pcl")))

(restas::define-site-plugin rulisp-forum (:rulisp.forum 'rulisp-plugin-instance)
  (rulisp.forum::*baseurl* ("forum")))

(restas::define-site-plugin rulisp-format (:rulisp.format 'rulisp-plugin-instance)
  (rulisp.format::*baseurl* ("apps" "format")))


(restas::define-site-plugin rulisp-planet (:rulisp.planet 'rulisp-plugin-instance))

(defun get-plugin (symbol)
  (gethash symbol *site-plugins*))


(defparameter rulisp.static::*mainmenu* `((,(get-plugin 'rulisp-static) rulisp.static::main "Главная")
                                          (,(get-plugin 'rulisp-static) rulisp.static::articles "Статьи")
                                          (,(get-plugin 'rulisp-planet) rulisp.planet::planet-main "Планета")
                                          (,(get-plugin 'rulisp-forum) rulisp.forum::forum-main "Форум")
                                          (,(get-plugin 'rulisp-static) rulisp.static::tools-list "Сервисы")
                                          (,(get-plugin 'rulisp-pcl) rulisp.pcl::pcl-main "Practical Common Lisp")
                                          (,(get-plugin 'rulisp-wiki) rulisp.wiki::wiki-main-page "wiki")))
  


;; (defparameter rulisp.static::*mainmenu* '((main "Главная")
;;                                           (articles "Статьи")
;;                                           (rulisp.planet::planet-main "Планета")
;;                                           (rulisp.forum::forum-main "Форум")
;;                                           (tools-list "Сервисы")
;;                                           (rulisp.pcl::pcl-main "Practical Common Lisp")
;;                                           (rulisp.wiki::wiki-main-page "wiki")))