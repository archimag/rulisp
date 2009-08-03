;;; test.lisp

(in-package :rulisp)


(xpath:define-xpath-function colorize (code)
  (code-to-html code))

(xslt:define-xslt-element text2html (self input output)
  (let ((text (xpath:find-string input
                                 (xtree:attribute-value self "select"))))
    (if text
        (html:with-parse-html (doc text)
          (let ((root (or (xpath:find-single-node (xtree:root doc) "body")
                                             (xtree:root doc))))
          (iter (for node in-child-nodes root)
                (xtree:append-child output (xtree:copy node))))))))

(xslt:defxsl *content-xsl* (merge-pathnames "xsl/content.xsl" *rulisp-path*))
(xslt:defxsl *articles-xsl* (merge-pathnames "xsl/articles.xsl" *rulisp-path*))

(defun apply-xsl (style obj)
  (let ((xpath:*lisp-xpath-functions* `((colorize "colorize" ,*rulisp-ns*)))
        (xslt:*lisp-xslt-elements* `((text2html "text2html" ,*rulisp-ns*))))
    (in-pool (xslt:transform style
                             (merge-pathnames obj *basepath*)))))

(define-simple-route main (""
                           :overlay-master *master*)
  (apply-xsl *content-xsl* "content/index.xml"))


(define-simple-route css ("css/:(file)")
  (skinpath (format nil "css/~A" file)))

(define-simple-route image ("image/:(file)")
  (skinpath (format nil "image/~A" file)))

(define-simple-route js ("js/:(file)")
  (skinpath (format nil "js/~A" file)))

(define-simple-route articles ("articles/"
                              :overlay-master *master*)
  (apply-xsl *content-xsl*
             "content/articles/index.xml"))

(define-simple-route article ("articles/:(file).html"
                              :overlay-master *master*)
  (apply-xsl *articles-xsl*
             (format nil "content/articles/~A.xml" file)))

(define-simple-route favicon ("favicon.ico")
  (skinpath "favicon.ico"))

