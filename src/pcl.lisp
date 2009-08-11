;;; pcl.lisp

(in-package :rulisp)

(defparameter *test-pcl-page-path*
  (merge-pathnames "content/macros-standard-control-constructs.dokuwiki" *basepath*))

(defparameter *test-pcl-page*  (wiki-parser:parse :dokuwiki *test-pcl-page-path*))


(defparameter *wiki-render-map* (make-hash-table))

(defun render-wiki-item (item)
  (cond
    ((and (consp item)
          (symbolp (car item))) (let ((render (gethash (car item) *wiki-render-map*)))
                                  (if render
                                      (funcall render (cdr item))
                                      (render-wiki-item (cdr item)))))
    ((consp item) (iter (for i in (cdr item))
                        (render-wiki-item i)))
    ((symbolp item) (let ((render (gethash item *wiki-render-map*)))
                      (if render
                          (funcall render nil))))
    ((stringp item) (xfactory:text item))))

(defun render-wiki-page (wikidoc)
  (xfactory:with-document-factory ((E))
    (E :overlay
       (E :div
          (eid "content")
          (E :div
             (render-wiki-item wikidoc))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-all-wiki-items (items)
  (iter (for item in items)
        (render-wiki-item item)))


(defmacro define-wiki-render (name (items) &body body)
  `(setf (gethash ',name
                  *wiki-render-map*)
         (lambda (,items)
           ,@body)))

(define-wiki-render dokuwiki:toplevel (items)
  (render-all-wiki-items items))
  
(define-wiki-render dokuwiki:chapter (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node*
                                                   "div")))
    (eclass "chapter")
    (render-all-wiki-items items)))


(define-wiki-render dokuwiki:header (items)
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                                   "h3")
                         (car items)))

(define-wiki-render dokuwiki:eol (items)
  (declare (ignore items))
  (e-break-line))

(define-wiki-render dokuwiki:footnote (items)
  (declare (ignore items))
  (estrong "'footnote - fix me'")
  )


(define-wiki-render dokuwiki:monospace (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "code")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:strong (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "strong")))
    (render-all-wiki-items items)))


(define-wiki-render dokuwiki:code (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "pre")))
    (eclass "code")
    (e-text2html (code-to-html (car items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route pcl-main ("pcl"
                               :overlay-master *master*)
  (in-pool
   (render-wiki-page *test-pcl-page*)))
