;;; test.lisp

(in-package :rulisp)

(defparameter *rulisp-site* (make-instance 'restas:site))

(defparameter *site-plugins* (make-hash-table))

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


(restas::define-site-plugin rulisp-static (:rulisp.static 'rulisp-plugin-instance))


(progn
  (setf (slot-value *rulisp-site* 'restas::plugins)
        nil)
  
  (restas:site-add-plugin *rulisp-site* (make-instance 'rulisp-plugin-instance
                                                       :plugin :rulisp.static))

  (restas:site-add-plugin *rulisp-site* (make-instance 'rulisp-plugin-instance
                                                       :plugin :rulisp.account))


;;   (restas:site-add-plugin *rulisp-site* (make-instance 'rulisp-plugin-instance
;;                                                        :plugin :rulisp.format
;;                                                        :context (let ((ctnx (restas::make-preserve-context)))
;;                                                                   (restas::context-add-variable ctnx 'rulisp.format::*baseurl*)
;;                                                                   (setf (restas::context-symbol-value ctnx 'rulisp.format::*baseurl*)
;;                                                                         '("format"))
;;                                                                   ctnx)))
  (restas::reconnect-all-sites))

;; (xpath:define-xpath-function colorize (code)
;;   (code-to-html code))

;; (xslt:define-xslt-element text2html (self input output)
;;   (let ((text (xpath:find-string input
;;                                  (xtree:attribute-value self "select"))))
;;     (if text
;;         (html:with-parse-html (doc text)
;;           (let ((root (or (xpath:find-single-node (xtree:root doc) "body")
;;                                              (xtree:root doc))))
;;           (iter (for node in-child-nodes root)
;;                 (xtree:append-child output (xtree:copy node))))))))

;; (xslt:defxsl *content-xsl* (merge-pathnames "src/xsl/content.xsl" *rulisp-path*))
;; (xslt:defxsl *articles-xsl* (merge-pathnames "src/xsl/articles.xsl" *rulisp-path*))

;; (defun apply-xsl (style obj)
;;   (let ((xpath:*lisp-xpath-functions* `((colorize "colorize" ,*rulisp-ns*)))
;;         (xslt:*lisp-xslt-elements* `((text2html "text2html" ,*rulisp-ns*)))
;;         (path (merge-pathnames obj *basepath*)))
;;     (if (fad:file-exists-p path)
;;         (in-pool (xslt:transform style
;;                                  (in-pool (xtree:parse path :xml-parse-noent ))))
;;         hunchentoot:+HTTP-NOT-FOUND+)))

;; (define-simple-route main (""
;;                            :overlay-master *master*)
;;   (apply-xsl *content-xsl* "content/index.xml"))


;; (define-simple-route css ("/css/:(theme)/:(file)")
;;   (skinpath (format nil "css/~A" file)
;;             theme))

;; (define-simple-route image ("image/:(file)")
;;   (staticpath (format nil "image/~A" file)))

;; (define-simple-route js ("js/:(file)")
;;   (staticpath (format nil "js/~A" file)))

;; (define-simple-route articles ("articles/"
;;                               :overlay-master *master*)
;;   (apply-xsl *content-xsl*
;;              "content/articles/index.xml"))

;; (define-simple-route article ("articles/:(afile)"
;;                               :overlay-master *master*)
;;   (let ((afile-length (length afile)))
;;     (if (and (> afile-length 4)
;;              (string= (subseq afile (- afile-length 5))
;;                       ".html"))
;;         (redirect 'article :afile (subseq afile 0 (- afile-length 5)))
;;         (apply-xsl *articles-xsl*
;;                    (format nil "content/articles/~A.xml" afile)))))

;; (define-simple-route favicon ("favicon.ico")
;;   (staticpath "favicon.ico"))



;; (defparameter *mainmenu* '((main "Главная")                           
;;                            (articles "Статьи")
;;                            (planet-main "Планета")
;;                            (forum-main "Форум")                           
;;                            (tools-list "Сервисы")
;;                            (pcl-main "Practical Common Lisp")
;;                            (wiki-main-page "wiki")))

;; (define-simple-route mainmenu ("mainmenu"
;;                                :protocol :chrome)
;;   (in-pool
;;    (xfactory:with-document-factory ((E))
;;      (E :ul
;;         (iter (for (route name) in *mainmenu*)
;;               (E :li
;;                  (E :a
;;                     (ehref route)
;;                     (xfactory:text name))))))))


;; (define-simple-route theme-css-include ("theme/css/:(file)"
;;                                         :protocol :chrome)
;;   (format nil
;;           "<link href=\"~A\" rel=\"stylesheet\" type=\"text/css\" />"
;;           (genurl 'css :theme (user-theme (username)) :file file)))
  
