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

(defun remove-obsolete-records ()
  (with-db-storage *rulisp-db-storage*
    (values 
     (postmodern:execute "delete from users  using confirmations
                                 where users.user_id = confirmations.user_id
                                 and (now() - confirmations.created) > interval '3 days'")
     (postmodern:execute "delete from forgot where (now() - created) > interval '3 day'"))))

(clon:schedule-function 'remove-obsolete-records
                        (clon:make-scheduler (clon:make-typed-cron-schedule :day-of-month '*)
                                             :allow-now-p t)
                        :thread t)

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
;;; forum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; storage-admin-p

(defmethod restas.forum:storage-admin-p ((storage rulisp-db-storage) user)
  (member user '("archimag" "lispnik" "turtle")
          :test #'string=))

;;;; storage-list-forums

(defmethod restas.forum:storage-list-forums ((storage rulisp-db-storage))
  (with-db-storage storage
    (postmodern:query (:order-by 
                       (:select 'pretty-forum-id 'description
                                :from 'rlf-forums)
                       'forum-id))))

;;;; storage-list-topics

(postmodern:defprepared select-topics*
    " SELECT fm.author as author, t.title, 
             to_char(fm.created, 'DD.MM.YYYY HH24:MI') as date,
             t.topic_id, t.all_message,
             m.author AS last_author,
             to_char(m.created, 'DD.MM.YYYY HH24:MI') AS last_created,
             fm.message_id AS first_author
        FROM rlf_topics AS t
        LEFT JOIN rlf_messages  AS m ON t.last_message = m.message_id
        LEFT JOIN rlf_messages AS fm ON t.first_message = fm.message_id
        LEFT JOIN rlf_forums AS f ON t.forum_id = f.forum_id
        WHERE f.pretty_forum_id = $1
        ORDER BY COALESCE(m.created, fm.created) DESC
        LIMIT $3 OFFSET $2")

(defmethod restas.forum:storage-list-topics ((storage rulisp-db-storage) forum limit offset)
  (with-db-storage storage
    (iter (for (author title created id message-count last-author last-date first-author) in (select-topics* forum offset limit))
          (collect (list :author author
                         :title title
                         :create-date created
                         :id id
                         :message-count message-count
                         :last-author (postmodern:coalesce last-author)
                         :last-date (postmodern:coalesce last-date))))))

;;; storage-create-topic

(defmethod restas.forum:storage-create-topic ((storage rulisp-db-storage) forum-id title body user)
  (with-db-storage storage
    (postmodern:query (:select (:rlf-new-topic forum-id title body user)))))

;;; storage-delete-topic

(defmethod restas.forum:storage-delete-topic ((storage rulisp-db-storage) topic)
  (with-db-storage storage
    (let ((forum-id (postmodern:query (:select '* :from (:rlf_delete_topic topic))
                                      :single)))
      (if (eql forum-id :null)
          nil
          forum-id))))

;;;; storage-form-info

(defmethod restas.forum:storage-forum-info ((storage rulisp-db-storage) forum)
  (with-db-storage storage
    (postmodern:query (:select 'description 'all-topics
                               :from 'rlf-forums
                               :where (:= 'pretty-forum-id forum))
                      :row)))

;;;; storage-topic-message

(defmethod restas.forum:storage-topic-message ((storage rulisp-db-storage) topic-id)
  (with-db-storage storage
    (bind:bind (((title id message-id all-message author body created) 
                 (postmodern:query (:select (:dot :t 'title)
                                            (:dot :t 'topic-id)
                                            (:dot :m 'message-id)
                                            (:dot :t 'all-message)
                                            (:dot :m 'author)
                                            (:as (:dot :m 'message) 'body)
                                            (:as (:to-char (:dot :m 'created) "DD.MM.YYYY HH24:MI") 'date)
                                            :from (:as 'rlf-topics :t)
                                            :left-join (:as 'rlf-messages :m) :on (:= (:dot :t 'first-message)
                                                                                      (:dot :m 'message-id))
                                            :where (:= (:dot :t 'topic-id)
                                                       topic-id))
                                   :row)))
      (list :title title
            :id id
            :message-id message-id
            :count-replies all-message
            :author author
            :created created
            :body body
            :forum (postmodern:query 
                    (:select 'pretty_forum_id 'description
                             :from 'rlf_forums
                             :where (:= 'forum_id (:select 'forum_id
                                                           :from 'rlf_topics
                                                           :where (:= 'topic_id topic-id))))
                    :row)))))

;;;; storage-topic-reply-count

(defmethod restas.forum:storage-topic-reply-count ((storage rulisp-db-storage) topic)
  (with-db-storage storage
    (postmodern:query (:select (:count '*) :from 'rlf-messages
                               :where (:= 'topic-id topic))
                      :single)))

;;;; storage-topic-replies

(defmethod restas.forum:storage-topic-replies ((storage rulisp-db-storage) topic limit offset)
  (with-db-storage storage
    (postmodern:query (:limit 
                       (:order-by 
                        (:select (:as (:dot :t1 'message-id) 'id)
                                 (:as (:dot :t1 'message) 'body)
                                 (:dot :t1 'author)
                                 (:as (:to-char (:dot :t1 'created) "DD.MM.YYYY HH24:MI") 'date)
                                 (:dot :t1 'reply-on)
                                 (:as (:dot :t2 'author) 'prev-author)
                                 (:as (:dot :t2 'message-id) 'prev-id)
                                 (:as (:to-char (:dot :t2 'created) "DD.MM.YYYY HH24:MI") 'prev-created)
                              :from (:as 'rlf-messages :t1)
                              :left-join (:as 'rlf-messages :t2) :on (:= (:dot :t1 'reply-on)
                                                                         (:dot :t2 'message-id))
                              :where (:= (:dot :t1 'topic-id) topic))
                        (:dot :t1 'created))
                       limit
                       (1+ offset))
                      :plists)))

;;;; storage-create-reply

(defmethod restas.forum:storage-create-reply ((storage rulisp-db-storage) reply-on body user)
  (with-db-storage storage
    (let ((message-id (postmodern:query (:select (:nextval "rlf_messages_message_id_seq"))
                                        :single)))
      (postmodern:execute (:insert-into 'rlf-messages
                                        :set
                                        'message-id message-id
                                        'topic-id (:select 'topic-id :from 'rlf-messages :where (:= 'message-id reply-on))
                                        'reply-on reply-on
                                        'message body
                                        'author user))
      message-id)))

;;;; storage-delete-reply

(defmethod restas.forum:storage-delete-reply ((storage rulisp-db-storage) reply)
  (let ((topic-id (with-db-storage storage
                    (postmodern:query (:select '* :from (:rlf_delete_message reply))
                                      :single))))
    (if (eql topic-id :null)
        nil
        topic-id)))

;;;; storage-reply-position

(defmethod restas.forum:storage-reply-position ((storage rulisp-db-storage) reply)
  (with-db-storage storage
    (let ((topic-id (postmodern:query (:select 'topic-id :from 'rlf-messages
                                               :where (:= 'message-id reply))
                                      :single)))
      (values (postmodern:query (:select (:count '*) 
                                         :from 'rlf-messages
                                         :where (:and (:= 'topic-id topic-id)
                                                      (:< 'created (:select 'created :from 'rlf-messages :where (:= 'message-id reply)))))
                                :single)
              topic-id))))
  
;;;; forum news (RSS)

(defmacro new-messages (where limit)
  `(with-db-storage storage
     (postmodern:query (:limit
                        (:order-by
                         (:select 'pretty-forum-id
                                  (:dot :m 'topic-id)
                                  (:dot :m 'author)
                                  (:dot :m 'message)
                                  (:as (:dot :m 'message-id) 'id)
                                  (:as (:to-char (:raw "created AT TIME ZONE 'GMT'")
                                                 "DY, DD Mon YYYY HH24:MI:SS GMT")
                                       'date)
                                  'title
                                  :from (:as 'rlf-messages :m)
                                  :left-join (:as 'rlf-topics :t) :on (:= (:dot :m 'topic-id)
                                                                          (:dot :t 'topic-id))
                                  :left-join (:as 'rlf-forums :f) :on (:= (:dot :t 'forum-id)
                                                                          (:dot :f 'forum-id))
                                  ,@(if where
                                        `(:where ,where)))
                         (:desc 'created))
                        ,limit)
                       :plists)))

(defmethod restas.forum:storage-all-news ((storage rulisp-db-storage) limit)
  (new-messages nil limit))

(defmethod restas.forum:storage-forum-news ((storage rulisp-db-storage) forum limit)
  (new-messages (:= (:dot :f 'pretty-forum-id)
                    forum)
            limit))

(defmethod restas.forum:storage-topic-news ((storage rulisp-db-storage) topic limit)
  (new-messages (:= (:dot :m 'topic-id)
                    topic)
            limit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pastebin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod restas.colorize:storage-count-notes ((storage rulisp-db-storage))
  (with-db-storage storage
    (postmodern:query (:select (:count '*) :from 'formats)
                      :single)))

(postmodern:defprepared select-formats*
    "SELECT f.format_id, u.login, f.title, f.created AT TIME ZONE 'GMT' FROM formats AS f
    LEFT JOIN users AS u USING (user_id)
    ORDER BY f.created DESC
    LIMIT $2 OFFSET $1")

(defmethod restas.colorize:storage-list-notes ((storage rulisp-db-storage) offset limit)
  (with-db-storage storage
    (iter (for item in (select-formats* offset limit))
          (collect (make-instance 'restas.colorize:note
                                  :id (first item)
                                  :author (second item)
                                  :title (third item)
                                  :date (local-time:universal-to-timestamp (simple-date:timestamp-to-universal-time (fourth item))))))))

(postmodern:defprepared get-note*
    "SELECT u.login, f.title, f.code, f.created AT TIME ZONE 'GMT', f.lang FROM formats AS f
     LEFT JOIN users AS u USING (user_id)
     WHERE format_id = $1"
  :row)

(defmethod restas.colorize:storage-get-note ((storage rulisp-db-storage) id)
  (with-db-storage storage
    (let ((raw (get-note* id)))
      (make-instance 'restas.colorize:note
                     :id id
                     :author (first raw)
                     :title (second raw)
                     :code (third raw)
                     :date (local-time:universal-to-timestamp (simple-date:timestamp-to-universal-time  (fourth raw)))
                     :lang (fifth raw)))))

(defmethod restas.colorize:storage-add-note ((storage rulisp-db-storage) note)
  (with-db-storage storage
    (let ((id (postmodern:query (:select (:nextval "formats_format_id_seq"))
                                :single))
          (user-id (postmodern:query (:select 'user-id :from 'users
                                              :where (:= 'login (restas.colorize:note-author note)))
                                     :single)))
      (postmodern:execute (:insert-into 'formats :set
                                        'format-id id
                                        'user-id user-id
                                        'title (restas.colorize:note-title note)
                                        'code (restas.colorize:note-code note)
                                        'lang (restas.colorize:note-lang note)))
      (setf (restas.colorize:note-id note)
            id))
    note))
