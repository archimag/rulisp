;;;; storage.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rulisp)

(defclass rulisp-db-storage ()
  ((dbspec :initarg :spec :initform nil)))

(defparameter *rulisp-db-storage*
  (make-instance 'rulisp-db-storage
                 :spec *rulisp-db*))

(defmacro with-db-storage (storage &body body)
  `(postmodern:with-connection (slot-value ,storage 'dbspec)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(postmodern:defprepared check-user-password*
    "SELECT (count(*) > 0) FROM users WHERE login = $1 AND password = $2 AND status IS NULL"
  :single)


(defmethod restas.simple-auth:storage-check-user-password ((storage rulisp-db-storage) login password)
  (with-db-storage storage
    (if (check-user-password* login password)
        login)))

(postmodern:defprepared check-email-exist*
    "select email from users where email = $1"
  :single)
    
(defmethod restas.simple-auth:storage-email-exist-p ((storage rulisp-db-storage) email)
    (with-db-storage storage
      (check-email-exist* email)))

(postmodern:defprepared check-login-exist*
    "select login from users where login = $1"
  :single)

(defmethod restas.simple-auth:storage-user-exist-p ((storage rulisp-db-storage) login)
  (with-db-storage storage
    (check-login-exist* login)))

(postmodern:defprepared db-add-new-user "SELECT add_new_user($1, $2, $3, $4)" :single)

(defmethod restas.simple-auth:storage-create-invite ((storage rulisp-db-storage) login email password)
  (let ((invite (calc-sha1-sum (format nil "~A~A~A" login email password))))
    (with-db-storage storage
      (db-add-new-user login email password invite))
    invite))

(defmethod restas.simple-auth:storage-invite-exist-p ((storage rulisp-db-storage) invite)
  (with-db-storage storage
    (postmodern:query (:select 'mark :from 'confirmations :where (:= 'mark invite))
                      :single)))


(defmethod restas.simple-auth:storage-create-account ((storage rulisp-db-storage) invite)
  (with-db-storage storage
    (let* ((account (postmodern:query (:select 'users.user_id 'login 'email 'password
                                               :from 'users
                                               :left-join 'confirmations :on (:= 'users.user_id 'confirmations.user_id)
                                               :where (:= 'mark invite))
                                      :row)))
      (postmodern:with-transaction ()
        (postmodern:execute (:update 'users 
                                     :set 'status :null  
                                     :where (:= 'user_id (first account))))
        (postmodern:execute (:delete-from 'confirmations
                                          :where (:= 'mark invite))))
      (cdr account))))

(defmethod restas.simple-auth:storage-create-forgot-mark ((storage rulisp-db-storage)  login-or-email)
  (with-db-storage storage
    (let ((login-info (postmodern:query (:select 'user-id 'login 'email :from 'users
                                                 :where (:and (:or (:= 'email login-or-email)
                                                                   (:= 'login login-or-email))
                                                              (:is-null 'status)))
                                        :row)))
      (if login-info
          (let ((mark (calc-sha1-sum (write-to-string login-info))))
            (postmodern:execute (:insert-into 'forgot
                                              :set 'mark mark 'user_id (first login-info)))
            (values mark
                    (second login-info)
                    (third login-info)))))))

(defmethod restas.simple-auth:storage-forgot-mark-exist-p ((storage rulisp-db-storage) mark)
  (with-db-storage storage
    (postmodern:query (:select 'mark
                               :from 'forgot
                               :where (:= 'mark  mark))
                      :single)))

(defmethod restas.simple-auth:storage-change-password ((storage rulisp-db-storage) mark password)
  (with-db-storage storage
    (postmodern:with-transaction ()
      (postmodern:execute (:update 'users
                                   :set 'password password
                                   :where (:= 'user_id (:select 'user_id
                                                                :from 'forgot
                                                                :where (:= 'mark mark)))))
      (postmodern:execute (:delete-from 'forgot :where (:= 'mark mark))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pastes 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod restas.colorize:count-all-pastes ((storage rulisp-db-storage))
  (with-db-storage storage
    (postmodern:query (:select (:count '*) :from 'formats)
                      :single)))

(postmodern:defprepared select-formats*
  "SELECT f.format_id, u.login, f.title, f.created AT TIME ZONE 'GMT' FROM formats AS f
    LEFT JOIN users AS u USING (user_id)
    ORDER BY f.created DESC
    LIMIT $2 OFFSET $1")

(defmethod restas.colorize:list-pastes ((storage rulisp-db-storage) offset limit)
  (with-db-storage storage
    (iter (for item in (select-formats* offset limit))
          (collect (make-instance 'restas.colorize:paste
                                  :id (first item)
                                  :author (second item)
                                  :title (third item)
                                  :date (local-time:universal-to-timestamp (fourth item)))))))

(postmodern:defprepared get-paste*
    "SELECT u.login, f.title, f.code, f.created AT TIME ZONE 'GMT', f.lang FROM formats AS f
     LEFT JOIN users AS u USING (user_id)
     WHERE format_id = $1"
  :row)

(defmethod restas.colorize:get-paste ((storage rulisp-db-storage) id)
  (with-db-storage storage
    (let ((raw (get-paste* id)))
      (make-instance 'restas.colorize:paste
                     :id id
                     :author (first raw)
                     :title (second raw)
                     :code (third raw)
                     :date (local-time:universal-to-timestamp (fourth raw))
                     :lang (fifth raw)))))

(defmethod restas.colorize:add-paste ((storage rulisp-db-storage) paste)
  (with-db-storage storage
    (let ((id (postmodern:query (:select (:nextval "formats_format_id_seq"))
                                :single))
          (user-id (postmodern:query (:select 'user-id :from 'users
                                              :where (:= 'login (restas.colorize:paste-author paste)))
                                     :single)))
      (postmodern:execute (:insert-into 'formats :set
                                        'format-id id
                                        'user-id user-id
                                        'title (restas.colorize:paste-title paste)
                                        'code (restas.colorize:paste-code paste)
                                        'lang (restas.colorize:paste-lang paste)))
      (setf (restas.colorize:paste-id paste)
            id))
      paste))
