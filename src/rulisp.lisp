;;; test.lisp

(in-package :rulisp)

(defparameter *rulisp-site* (make-instance 'restas:site))

(setf (gethash "rulisp" restas:*sites*) *rulisp-site*)

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

(progn
  (setf (slot-value *rulisp-site* 'restas::plugins)
        nil)
  
  (restas:site-add-plugin *rulisp-site* (make-instance 'rulisp-plugin-instance
                                                       :plugin :rulisp.static))

  (restas:site-add-plugin *rulisp-site* (make-instance 'rulisp-plugin-instance
                                                       :plugin :rulisp.account))


  (restas:site-add-plugin *rulisp-site* (make-instance 'rulisp-plugin-instance
                                                       :plugin :rulisp.format
                                                       :context (let ((ctnx (restas::make-preserve-context)))
                                                                  (restas::context-add-variable ctnx 'rulisp.format::*baseurl*)
                                                                  (setf (restas::context-symbol-value ctnx 'rulisp.format::*baseurl*)
                                                                        '("format"))
                                                                  ctnx)))
  (restas::reconnect-all-sites))

