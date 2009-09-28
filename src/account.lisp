;;; account.lisp

(restas:define-plugin :rulisp.account
  (:use :cl :iter :rulisp :rulisp.preferences)
  (:export #:compute-user-login-name))

(in-package :rulisp.account)


(defparameter *cookie-auth-name* "userauth")

(defparameter *user-auth-cipher* (ironclad:make-cipher :blowfish :mode :ecb :key *cookie-cipher-key*))

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
    (ironclad:encrypt-in-place *user-auth-cipher*
                               result)
    (ironclad:byte-array-to-hex-string result)))



(defun user-auth-cookie-set (name password &key (version 1))
  (hunchentoot:set-cookie *cookie-auth-name*
                          :value (user-auth-cookie-encrypt name password :version version)
                          :path "/"
                          :expires (+ (get-universal-time) (* 60 60 24 4))
                          :http-only t))

;;; user-auth-cookie-get
    
(defun user-auth-cookie-unpack (str)
  (split-sequence:split-sequence #\| str))

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

(defun user-auth-cookie-decrypt (str)
  (ignore-errors
    (let ((result (hex-string-to-byte-array str)))
      (ironclad:decrypt-in-place *user-auth-cipher*
                                 result)
      (user-auth-cookie-unpack (rulisp::octets-to-string result)))))

(defun user-auth-cookie-get ()
  (let ((cookie (hunchentoot:cookie-in *cookie-auth-name*)))
    (if cookie
        (user-auth-cookie-decrypt cookie))))

;;; check-user-password

(postmodern:defprepared check-user-password*
    "SELECT (count(*) > 0) FROM users WHERE login = $1 AND password = $2 AND status IS NULL"
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
  (setf *bindings*
        (acons :user-login-name login *bindings*))
  (user-auth-cookie-set login password-md5 :version version))

(defun run-logout ()
  "Cleaer cookie with auth information"
  (setf (cdr (assoc :user-login-name
                    *bindings*))
        nil)
  (hunchentoot:set-cookie *cookie-auth-name*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route auth-info ("auth/info-panel"
                                :protocol :chrome
                                :login-status :not-logged-on)
  (expand-file (tmplpath "account/info-panel.xml")
               (acons :callback
                      (hunchentoot:url-encode (format nil
                                                      "http://~A~A"
                                                      (hunchentoot:host)
                                                      (hunchentoot:request-uri hunchentoot:*request*)))
                      *bindings*)))

(define-simple-route user-panel ("auth/info-panel"
                                :protocol :chrome
                                :login-status :logged-on)
  (expand-file (tmplpath "account/user-info.xml")
               (acons :user (username) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route login ("login"
                            :login-status :not-logged-on)
  (in-pool (xtree:parse (tmplpath "account/login.xml"))))

(define-simple-route login/post ("login"
                                 :login-status :not-logged-on
                                 :method :post)
  (let ((name (hunchentoot:post-parameter "name"))
        (password-md5 (calc-md5-sum (hunchentoot:post-parameter "password")))
        (done (hunchentoot:get-parameter "done")))
    (if (check-user-password name password-md5)
        (progn
          (run-login name password-md5)
          (redirect (if done
                        (hunchentoot:url-decode done)
                        "/")))
        (redirect 'login))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route LOGOUT ("logout"
                             :login-status :logged-on)
  (run-logout)
  (redirect (or (hunchentoot:header-in :referer hunchentoot:*request*)
                'login)))
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-form ()
  (tmplpath "account/register.xml"))

(define-simple-route registration ("register"
                                   :login-status :not-logged-on)
  (register-form))

(postmodern:defprepared check-login-exist "select login from users where login = $1" :single)
(postmodern:defprepared check-email-exist "select email from users where email = $1" :single)


(defun check-register-form (formdata)
  (let ((badform nil))
    (flet ((form ()
             (or badform
                 (fill-form (setf badform
                                  (gp:object-register (xtree:parse (register-form))
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
                                        "Пользователь с таким email уже существует"))
                ))
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


(defparameter *register-confirmation-status* 1)

(postmodern:defprepared db-add-new-user "SELECT add_new_user($1, $2, $3, $4)" :single)

(defun create-confirmation (login email password)
  (let* ((confirmation (calc-sha1-sum (format nil "~A~A~A" login email password))))
    (with-rulisp-db
      (db-add-new-user login email password confirmation))
    (send-noreply-mail email
                       "Потверждение регистрации"
                       (skinpath "mail/confirmation")
                       :host (hunchentoot:host)
                       :link (genurl 'registration-confirmation
                                     :mark (pathname-name confirmation)))))
    


(define-simple-route registration/post ("register"
                                        :login-status :not-logged-on
                                        :method :post)
  (let* ((formdata (hunchentoot:post-parameters hunchentoot:*request*))
         (check-form (check-register-form formdata)))
    (if check-form
        check-form
        (let* ((login (form-field-value formdata "name"))
               (email (form-field-value formdata "email"))
               (password (calc-md5-sum (form-field-value formdata "password"))))
          (create-confirmation login email password)
          (tmplpath "account/register-send-mail.xml")))))

(defun show-confirmation-form ()
  (in-pool
   (xtree:parse (expand-file (tmplpath "account/confirmation.xml")
                             (acons :recaptcha-pubkey *reCAPTCHA.publick-key* nil)))))

(defun register-mark-exist-p (mark)
  (with-rulisp-db
    (postmodern:query (:select 'mark :from 'confirmations :where (:= 'mark mark))
                      :single)))
  
(define-simple-route registration-confirmation ("register/confirmation/:(mark)"
                                                :login-status :not-logged-on)
  (if (register-mark-exist-p mark)
      (show-confirmation-form)
      hunchentoot:+HTTP-NOT-FOUND+))


(define-simple-route registration-confirmation/post ("register/confirmation/:(mark)"
                                                     :method :post
                                                     :login-status :not-logged-on)
  (if (and (register-mark-exist-p mark)
           (cl-recaptcha:verify-captcha (hunchentoot:post-parameter "recaptcha_challenge_field")
                                        (hunchentoot:post-parameter "recaptcha_response_field")
                                        (hunchentoot:real-remote-addr)
                                        :private-key *reCAPTCHA.privake-key*))
      (progn
        (with-rulisp-db
          (postmodern:with-transaction ()
            (postmodern:execute (:update 'users 
                                         :set 'status :null  
                                         :where (:= 'user_id
                                                    (:select 'user_id
                                                             :from 'confirmations
                                                             :where (:= 'mark mark)))))
            (postmodern:execute (:delete-from 'confirmations
                                              :where (:= 'mark mark)))))
        (tmplpath "account/success-register.xml"))
      (show-confirmation-form)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forgot password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun forgot-form ()
  (in-pool (xtree:parse (tmplpath "account/forgot.xml"))))

(define-simple-route forgot-password ("forgot-password"
                                      :login-status :not-logged-on)
  (forgot-form))

(define-simple-route forgot-password/post ("forgot-password"
                                      :login-status :not-logged-on
                                      :method :post)
  (let ((login-info (with-rulisp-db
                      (postmodern:query (:select 'user-id 'login 'email :from 'users
                                                 :where (:and (:= 'email (hunchentoot:post-parameter "email"))
                                                              (:is-null 'status)))
                                        :row))))
    (if login-info
        (let ((mark (calc-sha1-sum (write-to-string login-info))))
          (with-rulisp-db
            (postmodern:execute (:insert-into 'forgot
                                              :set 'mark mark 'user_id (first login-info))))
          (send-noreply-mail (third login-info)
                             "Восстановление пароля"
                             (expand-file (skinpath "mail/forgot")
                                          (acons :host (hunchentoot:host)
                                                 (acons :link (genurl-with-host 'reset-password :mark mark)
                                                        nil))))
          (tmplpath "account/forgot-send-email.xml"))
        (let ((badform (in-pool (xtree:parse (forgot-form)))))
          (fill-form badform (hunchentoot:post-parameters*))
          (form-error-message badform
                              "email"
                              "Пользователь с таким email не зарегестрирован")
          badform))))

(defun reset-password-form ()
  (tmplpath "account/reset-password.xml"))
  

(defun forgot-mark-exist-p (mark)
  (with-rulisp-db
    (postmodern:query (:select 'mark
                               :from 'forgot
                               :where (:= 'mark  mark))
                      :single)))

(define-simple-route reset-password ("forgot-password/:(mark)"
                                     :login-status :not-logged-on)
  (if (forgot-mark-exist-p mark)
      (reset-password-form)
      hunchentoot:+HTTP-NOT-FOUND+))

      
(define-simple-route reset-password/post ("forgot-password/:(mark)"
                                          :method :post
                                          :login-status :not-logged-on)
  (if (forgot-mark-exist-p mark)
      (let ((reset-form (in-pool (xtree:parse (reset-password-form))))
            (password (hunchentoot:post-parameter "password"))
            (repassword (hunchentoot:post-parameter "confirmation"))
            (success nil))
        (cond
          ((string= password "") (form-error-message reset-form
                                                     "password"
                                                     "Необходимо указать пароль"))
          ((< (length password) 8) (form-error-message reset-form
                                                     "password"
                                                     "Должно быть не менее 8 символов"))
          ((not (string= password repassword)) (form-error-message reset-form
                                                             "confirmation"
                                                             "Пароли не совпадают"))
          (t (setf success t)))
        (if success
            (progn
             (with-rulisp-db
               (postmodern:with-transaction ()
                 (postmodern:execute (:update 'users
                                              :set 'password (calc-md5-sum password)
                                              :where (:= 'user_id (:select 'user_id
                                                                           :from 'forgot
                                                                           :where (:= 'mark mark)))))
                 (postmodern:execute (:delete-from 'forgot :where (:= 'mark mark))))) 
              (tmplpath "account/reset-password-success.xml"))
            (progn
              (fill-form reset-form (hunchentoot:post-parameters*))
              reset-form)))
      hunchentoot:+HTTP-NOT-FOUND+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; profile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-route all-themes-for-include ("theme/all"
                                             :protocol :chrome)
  (xtree:serialize 
   (in-pool
   (xfactory:with-document-factory ((E))
     (E :ul
        (iter (for skin in (fad:list-directory *skindir*))
              (let ((skinname (car (last (pathname-directory skin)))))
                (unless (string= skinname
                                 "default")
                  (E :li
                     (E :input
                        (xfactory:attributes :type "radio"
                                             :value skinname
                                             :name "theme"))
                     (xfactory:text skinname))))))))
   :to-string))


(define-simple-route user-profile ("profile"
                                   :login-status :logged-on)
  (in-pool (xtree:parse (tmplpath "account/profile.xml"))))

(postmodern:defprepared set-user-theme* "UPDATE users SET theme = $2 WHERE login = $1")

(define-simple-route user-profile/post ("profile"
                                        :method :post
                                        :login-status :logged-on) 
  (let ((theme (hunchentoot:post-parameter "theme")))
    (when (and theme
               (not (string= theme "")))
      (with-rulisp-db
        (set-user-theme* (username) theme))))
  (redirect 'user-profile))