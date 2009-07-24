;;; core.lisp

(in-package :rulisp)

(defparameter *rulisp-path* (asdf:component-pathname (asdf:find-system  :rulisp)))

(defparameter *skindir* (merge-pathnames "skins/default/" *rulisp-path*))

(defparameter *master* (merge-pathnames "rulisp.html" *skindir*))

(xslt:defxsl *content-xsl* (merge-pathnames "xsl/content.xsl" *rulisp-path*))

(defparameter *rulisp-ns* "chrome://rulisp/")

(defparameter *rulisp-db* '("rulisp" "lisp" "123" "localhost"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xsl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(push '(colorize "colorize" *rulisp-ns*) *xpath-functions*)
(push '(text2html "text2html" *rulisp-ns*) *xslt-elements*)


;;; digest

(defun calc-digest-sum (val digest)
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence digest
                                                               (sb-ext:string-to-octets val))))

(defun calc-md5-sum (val)
  "Calc md5 sum of the val (string)"
  (calc-digest-sum val :md5))
  
(defun calc-sha1-sum (val)
  "Calc sha1 sum of the val (string)"
  (calc-digest-sum val :sha1))

;;; misc

(defun username ()
  "Return name of the user if he loggen on"
  (cdr (assoc :user-login-name *bindings*)))
