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
  (xtree:with-custom-resolvers (#'chrome-resolver)
    (xtree:with-object (res (xoverlay:apply-overlay (tmplpath "rulisp.html") doc :html t))
      (let ((str (xtree:serialize res :to-string :pretty-print t)))
        str))))


(restas::define-site-plugin rulisp-static (:rulisp.static 'rulisp-plugin-instance))
(restas::define-site-plugin rulisp-account (:rulisp.account 'rulisp-plugin-instance))
(restas::define-site-plugin rulisp-wiki (:rulisp.wiki 'rulisp-plugin-instance))
(restas::define-site-plugin rulisp-pcl (:rulisp.pcl 'rulisp-plugin-instance))
(restas::define-site-plugin rulisp-forum (:rulisp.forum 'rulisp-plugin-instance))

(restas::define-site-plugin rulisp-format (:rulisp.format 'rulisp-plugin-instance)
  (rulisp.format::*baseurl* ("apps" "format")))


(restas::define-site-plugin rulisp-planet (:rulisp.planet 'rulisp-plugin-instance))