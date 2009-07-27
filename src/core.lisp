;;; core.lisp

(in-package :rulisp)

(defparameter *master* (merge-pathnames "rulisp.html" *skindir*))

(defparameter *rulisp-ns* "chrome://rulisp/")

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

(push `(colorize "colorize" ,*rulisp-ns*) *xpath-functions*)
(push `(text2html "text2html" ,*rulisp-ns*) *xslt-elements*)


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


(defun in-pool (obj)
  (gp:object-register obj *request-pool*))


(defmacro with-rulisp-db (&body body)
  `(postmodern:with-connection *rulisp-db*
     ,@body))

;;; xfactory

(defun apply-format-aux (format args)
  (if args
      (apply #'format nil (cons format args))
      format))

(defun eid (format &rest args)
  "Make id attribute"
  (xfactory:attributes :id
                       (apply-format-aux format args)))

(defun eclass (format &rest args)
  "Make class attribute"
  (xfactory:attributes :class
                       (apply-format-aux format args)))

(defun ehref (format &rest args)
  "Make href attribute"
  (xfactory:attributes :href
                       (apply-format-aux format args)))

(defun estyle (format &rest args)
  "Make style attributes"
  (xfactory:attributes :style
                       (apply-format-aux format args)))

(defun escript (src &optional (type "text/javascript"))
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "script")))
    (xfactory:attributes :src src
                         :type type)))

(defun ecss (src)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "link")))
    (xfactory:attributes :href src
                         :rel "stylesheet"
                         :type "text/css")))

(defun e-break-line ()
  (xtree:make-child-element xfactory:*node* "br"))

(defun estrong (format &rest args)
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                          "strong")
                         (apply-format-aux format args)))

(defun e-text2html (text)
  (if text
      (html:with-parse-html (html text)
        (when html
          (iter (for node in (iter (for node in-child-nodes (xpath:find-single-node html "/html/body"))
                                   (collect node)))
                (xtree:detach node)
                (xtree:append-child xfactory:*node* node))))))