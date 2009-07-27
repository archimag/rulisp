;;; account.lisp

(in-package :rulisp)

(defparameter *cookie-auth-name* "userauth")

(defparameter *user-auth-cipher* (ironclad:make-cipher :blowfish :mode :ecb :key *cookie-cipher-key*))

(defun user-auth-cipher ()
  *user-auth-cipher*)

(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  (declare (type string string))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (let ((x (position char "0123456789abcdef" :test #'char-equal)))
               (or x (error "Invalid hex key ~A specified" string)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
         finally (return key)))))


;;; user-auth-cookie-set

(defun user-auth-cookie-pack (name password &key (version 1) date)
  (format nil
          "~A|~A|~A|~A"
          version
          name
          password
          (or date
              (get-universal-time))))

(defun user-auth-cookie-encrypt (name password &key (version 1) date)
  (let ((result (ironclad:ascii-string-to-byte-array (user-auth-cookie-pack name password :version version :date date))))
    (ironclad:encrypt-in-place (user-auth-cipher)
                               result)
    (ironclad:byte-array-to-hex-string result)))



(defun user-auth-cookie-set (name password &key (version 1))
  (hunchentoot:set-cookie *cookie-auth-name*
                          :value (user-auth-cookie-encrypt name password :version version)
                          :path "/"
                          :http-only t))

;;; user-auth-cookie-get
    
(defun user-auth-cookie-unpack (str)
  (split-sequence:split-sequence #\| str))

(defun user-auth-cookie-decrypt (str)
  (ignore-errors
    (let ((result (hex-string-to-byte-array str)))
      (ironclad:decrypt-in-place *user-auth-cipher*
                                 result)
      (user-auth-cookie-unpack (sb-ext:octets-to-string result)))))

(defun user-auth-cookie-get ()
  (let ((cookie (hunchentoot:cookie-in *cookie-auth-name*)))
    (if cookie
        (user-auth-cookie-decrypt cookie))))

;;; check-user-password

(postmodern:defprepared check-user-password*
    "SELECT (count(*) > 0) FROM users WHERE login = $1 AND password = $2"
  :single)

(defun check-user-password (login password)
  (postmodern:with-connection *rulisp-db*
    (if (check-user-password* login password)
        login)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-user-login-name ()
  "Return user name for *request*. Required by restas plugins system"
  (let ((auth-info (user-auth-cookie-get)))
    (if auth-info
        (check-user-password (second auth-info)
                             (third auth-info)))))
        

(defun run-login (login password-md5 &key (version 1) )
  "Set cookie for user name and password"
  (user-auth-cookie-set login password-md5 :version version))

(defun run-logout ()
  "Cleaer cookie with auth information"
  (hunchentoot:set-cookie *cookie-auth-name*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route auth-info ("auth/info-panel"
                                :protocol :chrome
                                :login-status :not-logged-on)
  (restas::expand-text (alexandria:read-file-into-string (merge-pathnames "auth/info-panel.xml" *skindir*))
               (acons :callback
                      (hunchentoot:url-encode (format nil
                                                      "http://~A~A"
                                                      (hunchentoot:host)
                                                      (hunchentoot:request-uri hunchentoot:*request*)
                                                      ))
                      *bindings*)))

(define-simple-route user-panel ("auth/info-panel"
                                :protocol :chrome
                                :login-status :logged-on)
  (restas:expand-text (alexandria:read-file-into-string (merge-pathnames "auth/user-info.xml" *skindir*))
                      (acons :user (username) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-filesystem-route login "login"
  (namestring (merge-pathnames "auth/login.xml" *skindir*))
  :overlay-master *master*)

(define-simple-route login/post ("login"
                                 :overlay-master *master*
                                 :login-status :not-logged-on
                                 :method :post)
  (let ((name (hunchentoot:post-parameter "name"))
        (password-md5 (calc-md5-sum (hunchentoot:post-parameter "password")))
        (done (hunchentoot:get-parameter "done")))
    (if (check-user-password name password-md5)
        (progn
          (run-login name password-md5)
          (hunchentoot:redirect (if done
                                    (hunchentoot:url-decode done)
                                    "/")))
        (hunchentoot:redirect "/login"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route LOGOUT ("logout"
                             :overlay-master *master*
                             :login-status :logged-on)
  (run-logout)
  (hunchentoot:redirect (or (hunchentoot:header-in :referer hunchentoot:*request*)
                            "/login")))
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *register-form* (skinpath "auth/register.xml"))

(define-filesystem-route registration "register" *register-form*
                         :overlay-master *master*
                         :login-status :not-logged-on)

(postmodern:defprepared check-login-exist "select login from users where login = $1" :single)
(postmodern:defprepared check-email-exist "select email from users where email = $1" :single)


(defun check-register-form (formdata)
  (let ((badform nil))
    (flet ((form ()
             (or badform
                 (fill-form (setf badform
                                  (gp:object-register (xtree:parse *register-form*)
                                                      *request-pool*))
                            formdata))))
      (with-rulisp-db
        (if (form-field-empty-p formdata "name")
            (form-error-message (form)
                                "name"
                                "Не указан логин")
            (if (check-login-exist (form-field-value formdata "name"))
                (form-error-message (form)
                                    "name"
                                    "Пользователь с таким логином уже существует")))
        (if (form-field-empty-p formdata "email")
            (form-error-message (form)
                                "email"
                                "Не указан email")
            (if (not (ppcre:scan *re-email-check*
                                 (string-downcase (form-field-value formdata "email"))))
                (form-error-message (form)
                                    "email"
                                    "Это не похоже на email")
                (if (check-email-exist (form-field-value formdata "email"))
                    (form-error-message (form)
                                        "email"
                                        "Пользователь с таким email уже существует"))))
        (if (form-field-empty-p formdata "password")
            (form-error-message (form)
                                "password"
                                "Необходимо ввести пароль")
            (if (< (length (form-field-value formdata "password")) 8)
                (form-error-message (form)
                                    "password"
                                    "Должно быть не менее 8 символов")))
        (unless (string= (form-field-value formdata "password")
                         (form-field-value formdata "re-password"))
          (form-error-message (form)
                              "re-password"
                              "Пароли не совпадают"))
        badform))))

(defun register-confirmation-pathname (mark)
  (merge-pathnames mark
                   (merge-pathnames "tmp/register/"
                                    *vardir*)))

(defun create-confirmation (login email password)
  (let* ((confirmation (register-confirmation-pathname (calc-sha1-sum (format nil "~A~A~A" login email password))))
         (path (register-confirmation-pathname confirmation)))
    (ensure-directories-exist path)
    (alexandria:write-string-into-file (write-to-string (list login email password))
                                       path
                                       :if-does-not-exist :create)
    (send-noreply-mail email
                       "Потверждение регистрации"
                       (skinpath "mail/confirmation")
                       :host (hunchentoot:host)
                       :link (genurl 'registration-confirmation :mark confirmation))))
    


(define-simple-route registration/post ("register"
                                        :login-status :not-logged-on
                                        :overlay-master *master*
                                        :method :post)
  (let* ((formdata (hunchentoot:post-parameters hunchentoot:*request*))
         (check-form (check-register-form formdata)))
    (if check-form
        check-form
        (let* ((login (form-field-value formdata "name"))
               (email (form-field-value formdata "email"))
               (password (calc-md5-sum (form-field-value formdata "password"))))
          (create-confirmation login email password)
          (skinpath "auth/success-register.xml")))))

(define-simple-route registration-confirmation ("register/confirmation/:(mark)"
                                                :overlay-master *master*
                                                :login-status :not-logged-on)
  (declare (ignore mark))
  (in-pool
   (xfactory:with-document-factory ((E))
       (E :overlay
          (E :div
             (eid "content")
             "Hello")))))