;;; test.lisp

(in-package :rulisp)

(defparameter *rulisp-ns* "chrome://rulisp/")

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

(xslt:defxsl *content-xsl* (merge-pathnames "src/xsl/content.xsl" *rulisp-path*))
(xslt:defxsl *articles-xsl* (merge-pathnames "src/xsl/articles.xsl" *rulisp-path*))

(defun apply-xsl (style obj)
  (let ((xpath:*lisp-xpath-functions* `((colorize "colorize" ,*rulisp-ns*)))
        (xslt:*lisp-xslt-elements* `((text2html "text2html" ,*rulisp-ns*))))
    (in-pool (xslt:transform style
                             (merge-pathnames obj *basepath*)))))

(define-simple-route main (""
                           :overlay-master *master*)
  (apply-xsl *content-xsl* "content/index.xml"))


(define-simple-route css ("/css/:(theme)/:(file)")
  (skinpath (format nil "css/~A" file)
            theme))

(define-simple-route image ("image/:(file)")
  (staticpath (format nil "image/~A" file)))

(define-simple-route js ("js/:(file)")
  (staticpath (format nil "js/~A" file)))

(define-simple-route articles ("articles/"
                              :overlay-master *master*)
  (apply-xsl *content-xsl*
             "content/articles/index.xml"))

(define-simple-route article ("articles/:(afile).html"
                              :overlay-master *master*)
  (apply-xsl *articles-xsl*
             (format nil "content/articles/~A.xml" afile)))

(define-simple-route favicon ("favicon.ico")
  (staticpath "favicon.ico"))



(defparameter *mainmenu* '((main "Главная")
                           (articles "Статьи")
                           (planet-main "Планета")
                           (forum-main "Форум")
                           (tools-list "Сервисы")))

(define-simple-route mainmenu ("mainmenu"
                               :protocol :chrome)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :ul
        (iter (for (route name) in *mainmenu*)
              (E :li
                 (E :a
                    (ehref route)
                    (xfactory:text name))))))))


(define-simple-route theme-css-include ("theme/css/:(file)"
                                        :protocol :chrome)
  (format nil
          "<link href=\"~A\" rel=\"stylesheet\" type=\"text/css\" />"
          (genurl 'css :theme (user-theme (username)) :file file)))
  