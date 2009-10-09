;;; messages.lisp

(in-package :rulisp.forum)

(postmodern:defprepared select-message*
    "SELECT t.title, t.topic_id, t.all_message, m.author, m.message as body,
            to_char(m.created, 'DD.MM.YYYY HH24:MI') as date
       FROM rlf_topics AS t
       LEFT JOIN rlf_messages AS m ON t.first_message = m.message_id
       WHERE t.topic_id = $1")

(defun select-message (topic-id)
  (car (select-message* topic-id)))

(postmodern:defprepared select-reply-list*
    "SELECT message_id,
            message,
            author,
            to_char(created, 'DD.MM.YYYY HH24:MI') as date
        FROM rlf_messages 
        WHERE topic_id = $1
        ORDER BY created ASC
        OFFSET 1")

(defun select-reply-list (topic-id)
  (select-reply-list* topic-id))

(defun get-forum-info (topic-id)
  (postmodern:query (:select 'pretty_forum_id 'description
                             :from 'rlf_forums
                             :where (:= 'forum_id (:select 'forum_id
                                                           :from 'rlf_topics
                                                           :where (:= 'topic_id topic-id))))
                    :row))

(define-route view-topic ("thread/:(topic-id)")
  (with-rulisp-db
    (bind:bind (((forum-id description) (get-forum-info topic-id))
                ((title message-id all-message author body date) (select-message topic-id))
                (reply-list (select-reply-list topic-id))
                (theme (user-theme (username))))
      (declare (ignore message-id))
      (in-pool
       (xfactory:with-document-factory ((E))
         (E :overlay
            (E :head
               (E :title
                  (xfactory:text title))
               (E :link
                  (xfactory:attributes :rel "alternate"
                                       :type "application/rss+xml"
                                       :title (format nil "Тема  '~A' - RSS-лента" title)
                                       :href (genurl 'topic-rss :topic-id topic-id)))
               (ecss 'rulisp::css :file "forum.css" :theme theme)
               (ecss 'rulisp::css :file  "jquery.wysiwyg.css" :theme theme)
               (escript "/js/jquery.js")
               (escript "/js/jquery.wysiwyg.js")
               (escript "/js/forum.js"))
            (E :div
               (eid "content")
               (E :div
                  (eid "forum-nav-panel")
                  (E :a
                     (eclass "rss-link")
                     (ehref 'topic-rss :topic-id topic-id)
                     "RSS")
                  (E :ul
                     (E :li
                        (E :a
                           (ehref 'forum-main)
                           "Список форумов"))
                     (E :li
                        (E :a
                           (ehref 'view-forum-main :forum-id forum-id)
                           (xfactory:text description)))
                     (E :li
                        (estrong (if (> (length title) 64)
                                     (subseq title 0 64)
                                     title)))))

               (E :div
                  (eclass "thread")

                  (E :div
                     (eclass "topic")
                     (E :div
                        (E :big
                           (xfactory:text title)))

                     (E :div
                        (eclass "topic-info")
                        (E :span
                           (eclass "topic-author")
                           "Автор: "
                           (estrong author)
                           (xfactory:text " - ~A, Сообщений - ~A" date all-message)))

                     (E :div
                        (eclass "topicbody"
                                (e-text2html body))))

                  (iter (for (message-id message author date) in reply-list)
                        (E :div
                           (eclass "reply")
                           (E :div
                              (eclass "topic-info")
                              (when (forum-admin-p (username))
                                (E :a
                                   (eclass "delete-this")
                                   (ehref 'delete-topic-message :message-id message-id)
                                   "Удалить"))
                              (E :span
                                 (eclass "topic-author")
                                 "Автор: "
                                 (estrong author)
                                 (xfactory:text " - ~A" date)))
                           (E :div
                              (eclass "replybody")
                              (e-text2html message)))))

               (when (username)
                 (E :input
                    (xfactory:attributes :type "button"
                                         :value "+ Новое сообщение"
                                         :onclick "newmessage()"))

                 (E :form
                    (xfactory:attributes :method "post"
                                         :class "newmessage"
                                         :style "display: none"
                                         :id "editor")
                    (E :div "Новое сообщение: ")
                    (E :div
                       (E :textarea
                          (xfactory:attributes :rows "30" :name "body" :id "wysiwyg")))
                    (E :dev
                       (E :input
                          (xfactory:attributes :type "submit" :value "Отправить")))))

               )))))))
  
(postmodern:defprepared insert-new-message
    "INSERT INTO rlf_messages (topic_id, message, author) VALUES($1, $2, $3)")

(define-route new-topic-message ("thread/:(topic-id)"
                                        :method :post
                                        :login-status :logged-on)
  (let ((body (hunchentoot:post-parameter "body")))
    (unless (string= body "")
      (with-rulisp-db
        (insert-new-message topic-id
                            body
                            (username))))
      (restas:redirect 'view-topic :topic-id topic-id)))


(define-route delete-topic-message ("message/delete/:(message-id)"
                                           :login-status :logged-on)
  (if (forum-admin-p (username))
      (with-rulisp-db
        (let ((topic-id (postmodern:query (:select '* :from (:rlf_delete_message message-id))
                                           :single)))
          (if (eql topic-id :null)
              (restas:redirect 'forum-main)
              (restas:redirect 'view-topic :topic-id topic-id))))
      hunchentoot:+HTTP-FORBIDDEN+))
