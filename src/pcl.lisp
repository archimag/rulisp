;;; pcl.lisp

(in-package :rulisp)

(defparameter *test-pcl-page-path*
  (merge-pathnames "content/macros-standard-control-constructs.dokuwiki" *basepath*))

(defparameter *test-pcl-page* (wiki-parser:parse :dokuwiki *test-pcl-page-path*))


;; (defun render-dokuwiki-page (page)
;;   (if (consp page)
;;       (co)
;;       kK""
    
;;  (iter (for item in page)
;;        (cond
          



(define-simple-route pcl-main ("pcl"
                               :overlay-master *master*)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :div
        (eid "content")
        (render-dokuwiki-page *test-pcl-page*)))))
  