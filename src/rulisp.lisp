;;; test.lisp

(in-package :rulisp-plugin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *restas-ns* "restas://restas/"))

(defun empty-line-p (line)
  (string= (string-trim #(#\Space #\Tab) line) ""))

(xpath:define-xpath-function colorize (code)
  (let ((lines (split-sequence:split-sequence #\Newline code)))
    (iter
      (while lines)
      (for isempty = (empty-line-p (first lines)))
      (if isempty
          (setf lines (cdr lines)))
      (while isempty))
    (iter
      (while lines)
      (for isempty = (empty-line-p (car (last lines))))
      (if isempty
          (setf lines
                (remove (car (last lines)) lines)))
      (while isempty))
    (let ((min-space-count (iter (for line in (remove-if #'empty-line-p lines))
                                 (minimize (or (position #\Space line :test-not #'char-equal) 0)))))
      (setf lines
            (iter (for line in lines)
                  (collect (if (empty-line-p line)
                               ""
                               (subseq line min-space-count))))))
    (colorize::html-colorization :common-lisp
                                 (format nil "~{~A~%~}" lines))))

(xslt:define-xslt-element text2html (self input output)
  (let ((text (xpath:find-string input
                                 (xtree:attribute-value self "select"))))
    (if text
        (html:with-parse-html (doc text)
          (let ((root (or (xpath:find-single-node (xtree:root doc) "body")
                                             (xtree:root doc))))
          (iter (for node in-child-nodes root)
                (xtree:append-child output (xtree:copy node))))))))

(push '(colorize "colorize" *restas-ns*) *xpath-functions*)
(push '(text2html "text2html" *restas-ns*) *xslt-elements*)

(xslt:defxsl *content-xsl* (merge-pathnames "xsl/content.xsl" *basepath*))

(defparameter/update *skindir* (merge-pathnames "skins/default/" *basepath*))

(defparameter/update *master* (merge-pathnames "rulisp.html" *skindir*))

(restas:define-fs-xsl-route main "" "content/index.xml" *content-xsl* :overlay-master *master*)

(restas:define-filesystem-route css "css/:(file)" (format nil "~A~A" "skins/default/" "css/${file}"))
(restas:define-filesystem-route css/image "css/image/:(file)" (format nil "~A~A" "skins/default/" "css/image/${file}"))

(restas:define-fs-xsl-route articles "articles/" "content/articles/index.xml" *content-xsl* :overlay-master *master*)

(restas:define-fs-xsl-route article "articles/:(file).html" "content/articles/${file}.xml" *content-xsl* :overlay-master *master*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-simple-route auth-info ("auth/info-panel" :protocol :chrome)
  (restas::expand-text (alexandria:read-file-into-string (merge-pathnames "auth/info-panel.xml" *skindir*))
               (acons :callback
                      (hunchentoot:url-encode (format nil
                                                      "http://~A~A"
                                                      (hunchentoot:host)
                                                      (hunchentoot:request-uri hunchentoot:*request*)))
                      bindings)))

(restas:define-filesystem-route login "login.html"
  (namestring (merge-pathnames "auth/login.xml" *skindir*))
  :overlay-master *master*)

(restas:define-filesystem-route registration "register.html" 
  (namestring (merge-pathnames "auth/register.xml" *skindir*)) 
  :overlay-master *master*)
