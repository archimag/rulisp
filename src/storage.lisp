;;;; storage.lisp

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


(defmethod restas.simple-auth:check-user-password (storage login password)
  (with-db-storage storage
    (if (check-user-password* login password)
        login)))
    


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
