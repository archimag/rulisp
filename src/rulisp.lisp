;;; test.lisp

(in-package :rulisp)

(define-fs-xsl-route main "" "content/index.xml" *content-xsl* :overlay-master *master*)

(define-filesystem-route css "css/:(file)" (format nil "~A~A" "skins/default/" "css/${file}"))
(define-filesystem-route css/image "css/image/:(file)" (format nil "~A~A" "skins/default/" "css/image/${file}"))

(define-fs-xsl-route articles "articles/" "content/articles/index.xml" *content-xsl* :overlay-master *master*)

(define-fs-xsl-route article "articles/:(file).html" "content/articles/${file}.xml" *content-xsl* :overlay-master *master*)


