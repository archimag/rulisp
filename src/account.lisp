;;; account.lisp

(in-package :rulisp)

(defparameter *cookie-cipher-key* (ironclad:ascii-string-to-byte-array "asdfpqwurqwueasdf"))

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

(define-filesystem-route registration "register" 
  (namestring (merge-pathnames "auth/register.xml" *skindir*)) 
  :overlay-master *master*)
