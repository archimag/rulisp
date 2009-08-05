;;; core.lisp

(in-package :rulisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xsl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun code-to-html (code)
  (flet ((empty-line-p (line)
           (string= (string-trim #(#\Space #\Tab) line) "")))
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
                                   (format nil "~{~A~%~}" lines)))))

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

(defun substring (text end)
  (if (> (length text) end)
      (subseq text 0 end)
      text))

(defun username ()
  "Return name of the user if he loggen on"
  (cdr (assoc :user-login-name *bindings*)))

(defun expand-file (path bindings)
  "Loads a template file and substitutes the value of the bindings"
  (restas::expand-text (alexandria:read-file-into-string path)
                       bindings))

(defun in-pool (obj)
  (gp:object-register obj *request-pool*))


(defmacro with-rulisp-db (&body body)
  `(postmodern:with-connection *rulisp-db*
     ,@body))

(defparameter *re-email-check* 
  "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$")

(defun apply-format-aux (format args)
  (if (symbolp format)
      (apply #'genurl format args)
      (if args
          (apply #'format nil (cons format args))
          format)))

(defun redirect (route-symbol &rest args)
  (let* ((url (apply-format-aux route-symbol args))
         (route (car (routes:match restas::*mapper*
                                   url
                                   (acons :method :get nil))))
         (required-login-status (restas::route-required-login-status route)))
    (hunchentoot:redirect (if (or (null required-login-status)
                                  (and (eql required-login-status :logged-on)
                                       (username))
                                  (and (eql required-login-status :not-logged-on)
                                       (null (username))))
                              url
                              "/"))))

(defun genurl-with-host (route &rest args)
  (format nil
          "http://~A~A"
          (hunchentoot:host)
          (apply #'genurl route args)))

;;; xfactory


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
  "Make script element"
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "script")))
    (xfactory:attributes :src src
                         :type type)))

(defun ecss (format &rest args)
  "Make link css element"
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "link")))
    (xfactory:attributes :href (apply-format-aux format args)
                         :rel "stylesheet"
                         :type "text/css")))

(defun e-break-line ()
  "Make br element"
  (xtree:make-child-element xfactory:*node* "br"))

(defun estrong (format &rest args)
  "Make strong element"
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                          "strong")
                         (apply-format-aux format args)))

(defun e-text2html (text)
  "parse text as html and append to current element"
  (if text
      (html:with-parse-html (html text)
        (when html
          (iter (for node in (iter (for node in-child-nodes (xpath:find-single-node html "/html/body"))
                                   (collect node)))
                (xtree:detach node)
                (xtree:append-child xfactory:*node* node))))))

;;; mail

(defvar *sendmail*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(defun send-mail (to head content)
  (let* ((sendmail-process (sb-ext:run-program *sendmail*
                                               to
                                               :input :stream
                                               :output nil
                                               :error nil
                                               :wait nil))
         (sendmail (sb-ext:process-input sendmail-process)))
    (unwind-protect
         (progn
           (iter (for head-line in (acons "To" (format nil "~{~A ~}" to)  head))
                 (format sendmail
                         "~A: ~A~%"
                         (car head-line)
                         (cdr head-line)))
           (format sendmail "Content-Type: text/html; charset=\"utf-8\"~%~%")
           (typecase content
             (xtree::libxml2-cffi-object-wrapper (xtree:serialize content sendmail))
             (string (write-string content sendmail))
             (pathname (write-string (alexandria:read-file-into-string content) sendmail)))
           t)
      (close sendmail)
      (sb-ext:process-wait sendmail-process)
      (sb-ext:process-close sendmail-process))))

(defun prepare-subject (subject &optional (external-format :utf-8))
  (format nil
          "=?~A?B?~A?="
          external-format
          (base64:string-to-base64-string (coerce (mapcar #'code-char
                                                          (coerce (sb-ext:string-to-octets subject :external-format external-format)
                                                                  'list))
                                                  'string))))

(defun send-noreply-mail (receiver subject body &rest bindings)
  (send-mail (list receiver)
             (acons "From"
                    *noreply-mail-account*
                    (acons "Subject"
                           (prepare-subject subject)
                           nil))
             (typecase body
               (pathname (expand-file body (alexandria:plist-alist bindings)))
               (string (restas::expand-text body (alexandria:plist-alist bindings)))
               (otherwise (error "bad mail body: ~A" body)))))

;;; check html form support

(defun form-error-message (form field text)
  (let ((el (xtree:make-element "div")))
    (setf (xtree:attribute-value el "class") "error-info")
    (setf (xtree:text-content el) text)
    (xtree:insert-child-before el
                               (or (xpath:find-single-node form
                                                           (format nil "//input[@name='~A']" field))
                                   (error "bad filed: ~A" field)))))


(defun form-field-value (formdata field)
  (cdr (assoc field formdata :test #'string=)))

(defun form-field-empty-p (formdata field)
  (string= (form-field-value formdata field) ""))

(defun fill-form (form formdata)
  (iter (for field in-xpath-result "//input|//textarea" on form)
        (let ((field-value (form-field-value formdata
                                             (xtree:attribute-value field "name"))))
          (if field-value
              (cond
                ((string= (xtree:local-name field) "textarea") (setf (xtree:text-content field) 
                                                                     (xtree:encode-special-chars nil field-value)))
                (t (setf (xtree:attribute-value field "value") 
                         (xtree:encode-special-chars nil field-value)))))))
  form)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *static-path* (merge-pathnames "static/" *basepath*))

(defun staticpath (path)
  (merge-pathnames path *static-path*))

(postmodern:defprepared user-theme* "SELECT theme FROM users WHERE login = $1" :single)

(defun user-theme (name)
  (let ((theme (if name
                   (with-rulisp-db (user-theme* name)))))
    (if (and theme
             (not (eql theme :null))
             (fad:directory-exists-p (merge-pathnames theme *skindir*)))
        theme
        *default-skin*)))

(defun skinpath (path &optional theme)
  (let ((result (merge-pathnames path
                                 (format nil "~A/~A/"  *skindir* (or theme
                                                                     (user-theme (username)))))))
    (if (fad:file-exists-p result)
        result
        (merge-pathnames path
                         (format nil "~A/default/"  *skindir*)))))

(defun tmplpath (path)
  (skinpath (merge-pathnames path "templates/")))

(defparameter *master*
  (lambda ()
    (tmplpath "rulisp.html")))
    ;;(in-pool (xtree:parse (tmplpath "rulisp.html") :xml-parse-xinclude :xml-parse-noxincnode))))

