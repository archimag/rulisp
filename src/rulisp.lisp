;;; test.lisp

(in-package :rulisp)

(xslt:defxsl *content-xsl* (merge-pathnames "xsl/content.xsl" *rulisp-path*))
(xslt:defxsl *articles-xsl* (merge-pathnames "xsl/articles.xsl" *rulisp-path*))

(define-fs-xsl-route main "" "content/index.xml" *content-xsl* :overlay-master *master*)

(define-filesystem-route css "css/:(file)" (format nil "~A~A" "skins/default/" "css/${file}"))
(define-filesystem-route css/image "css/image/:(file)" (format nil "~A~A" "skins/default/" "css/image/${file}"))

(define-filesystem-route js "js/:(file)" (format nil "~A~A" "skins/default/" "js/${file}"))

(define-fs-xsl-route articles "articles/" "content/articles/index.xml" *content-xsl* :overlay-master *master*)

(define-fs-xsl-route article "articles/:(file).html" "content/articles/${file}.xml" *articles-xsl* :overlay-master *master*)


