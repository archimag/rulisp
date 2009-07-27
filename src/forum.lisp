;;; forum.lisp

(in-package :rulisp)

(defun substring (text end)
  (if (> (length text) end)
      (subseq text 0 end)
      text))

(defun admin-p (name)
  (string= name "archimag"))

;;; main

(define-simple-route forum-main ("forum/"
                                 :overlay-master *master*)
  (postmodern:with-connection *rulisp-db*
    (in-pool
     (xfactory:with-document-factory ((E))
       (E :overlay
          (E :head
             (E :title "Форум")
             (E :link
                (xfactory:attributes :rel "alternate"
                                     :type "application/rss+xml"
                                     :title (format nil "Форумs '~A' - RSS-лента" (hunchentoot:host))
                                     :href (genurl 'all-forums-rss)))
             (ecss "/css/forum.css"))
          (E :div
             (eid "content")
             (iter (for (id description) in (postmodern:query "SELECT pretty_forum_id, description FROM rlf_forums"))
                   (E :div
                      (eclass "forum")
                      (E :a
                         (ehref id)
                         (xfactory:text description))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; topics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(postmodern:defprepared select-topics*
    " SELECT fm.author as author, t.title, fm.message as body,
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

(defun select-topics (forum-id start)
  (select-topics* forum-id
                  start
                  10))


(postmodern:defprepared forum-info "SELECT description, all_topics FROM rlf_forums WHERE pretty_forum_id = $1")


(defun topic-nav-panel (forum-id start end all)
  (let ((url (genurl 'view-forum-main :forum-id forum-id)))
    (xfactory:with-element-factory ((E))
      (E :span
         (estrong "~A" (1+ start))
         " - "
         (estrong "~A" end)
         " из "
         (estrong "~A" all)
         " « "
         (if (> start 0)
             (E :a
                (if (> start 10)
                    (ehref "~A?start=~A" url (- start 10))
                    (ehref url))
                "Позже")
             (xfactory:text "Позже"))
         " | "
         (if (< (+ start 10) all)
             (E :a
                (ehref "~A?start=~A" url (+ start 10))
                "Раньше")
             (xfactory:text "Раньше"))
         " » "))))
       
(defun show-forum-topics (forum-id &optional (start 0))
  (with-rulisp-db 
    (bind:bind (((description all-topics) (car (forum-info forum-id)))
                (last (max (+ 10 start) all-topics))
                (topics (select-topics forum-id start)))
      (xfactory:with-document-factory ((E))
        (E :div
           (E :head
              (E :title
                 (xfactory:text description))
              (E :link
                 (xfactory:attributes :rel "alternate"
                                      :type "application/rss+xml"
                                      :title (format nil "Форум '~A' - RSS-лента" description)
                                      :href (genurl 'forum-rss :forum-id forum-id)))
              (ecss "/css/forum.css")
              (ecss "/css/jquery.wysiwyg.css")
              (escript "/js/jquery.js")
              (escript "/js/jquery.wysiwyg.js")
              (escript "/js/forum.js"))           
           (E :div
              (eid "content")
              (E :div
                 (eid "forum-nav-panel")
                 (E :ul
                    (E :li
                       (E :a
                          (ehref (genurl 'forum-main))
                          "Список форумов"))
                    (E :li
                       (estrong description))))
              (E :div
                 "Темы "
                 (xfactory:attributes :class "info"
                                      :align "right")
                 (topic-nav-panel forum-id start last all-topics))

              (E :div
                 (eclass "topic-list")
                 (iter (for (author title body date topic-id all-message last-author last-created first-author)
                            in topics)
                       (E :div
                          (eclass "topic")
                          (when (admin-p (username))
                            (E :a
                               (eclass "delete-this")
                               (ehref (genurl 'delete-topic :topic-id topic-id))
                               "Удалить"))
                          (E :a
                             (ehref (genurl 'view-topic :topic-id topic-id))
                             (xfactory:text title))
                          (E :div
                             (eclass "topicbody")
                             (xfactory:text (html:with-parse-html (html body)
                                              (let ((str (xtree:text-content html)))
                                                (if (< (length str) 500)
                                                    str
                                                    (subseq str 0 500)))))
                             )
                          (E :div
                             (eclass "topic-info")
                             (E :span
                                (eclass "topic-author")
                                "Автор: "
                                (estrong author)
                                (xfactory:text " - ~A" date)
                                (e-break-line)
                                (xfactory:text "Сообщений: ~A" all-message)
                                (unless (eql last-author :null)
                                  (e-break-line)
                                  (xfactory:text "Последнее:  ")
                                  (estrong last-author)
                                  (xfactory:text " - ~A" last-created))
                                )))))
              
              (E :div
                 (xfactory:attributes :class "info"
                                      :align "right")
                 (topic-nav-panel forum-id start last all-topics))

              (when (username)
                (E :input
                   (xfactory:attributes :type "button"
                                        :value "+ Новая тема"
                                        :onclick "newmessage()"))

                (E :form
                   (xfactory:attributes :method "post"
                                        :class "newmessage"
                                        :style "display: none"
                                        :id "editor")
                   (E :div "Новая тема")
                   (E :div
                      (estrong "Тема:")
                      (E :input
                         (xfactory:attributes :name "title" :size "80")))
                   (E :div
                      (E :textarea
                         (xfactory:attributes :rows "30" :name "body" :id "wysiwyg")))
                   (E :dev
                      (E :input
                         (xfactory:attributes :type "submit" :value "Отправить")))))
              ))))))
               
                           
  
(define-simple-route view-forum-main ("forum/:(forum-id)"
                                      :overlay-master *master*)
  (in-pool
   (show-forum-topics forum-id
                      (let ((start (hunchentoot:get-parameter "start")))
                        (if start
                            (parse-integer start)
                            0)))))


(postmodern:defprepared insert-new-topic
    "select rlf_new_topic($1, $2, $3, $4)")

(define-simple-route new-forum-topic ("forum/:(forum-id)"
                                      :method :post
                                      :login-status :logged-on)
  (let ((title (hunchentoot:post-parameter "title"))
        (body (hunchentoot:post-parameter "body")))
    (unless (or (string= title "")
                (string= body ""))
      (with-rulisp-db
        (insert-new-topic forum-id title body (username)))))
  (hunchentoot:redirect (genurl 'view-forum-main :forum-id forum-id)))
  

(define-simple-route delete-topic ("forum/thread/delete/:(topic-id)"
                                   :login-status :logged-on)
  (if (admin-p (username))
      (with-rulisp-db
        (let ((forum-id (postmodern:query (format nil "SELECT * from rlf_delete_topic(~A)" topic-id) :single)))
          (hunchentoot:redirect (if (eql topic-id :null)
                                    (genurl 'forum-main)
                                    (genurl 'view-forum-main :forum-id forum-id)))))
      hunchentoot:+HTTP-FORBIDDEN+))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (car (postmodern:query (format nil
                                 "SELECT pretty_forum_id, description FROM rlf_forums
WHERE forum_id = (SELECT forum_id FROM rlf_topics WHERE topic_id = ~A)"
                                 topic-id))))



(define-simple-route view-topic ("forum/thread/:(topic-id)"
                                 :overlay-master *master*)
  (with-rulisp-db
    (bind:bind (((forum-id description) (get-forum-info topic-id))
                ((title message-id all-message author body date) (select-message topic-id))
                (reply-list (select-reply-list topic-id)))
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
               (ecss "/css/forum.css")
               (ecss "/css/jquery.wysiwyg.css")
               (escript "/js/jquery.js")
               (escript "/js/jquery.wysiwyg.js")
               (escript "/js/forum.js"))
            (E :div
               (eid "content")
               (E :div
                  (eid "forum-nav-panel")
                  (E :ul
                     (E :li
                        (E :a
                           (ehref (genurl 'forum-main))
                           "Список форумов"))
                     (E :li
                        (E :a
                           (ehref (genurl 'view-forum-main :forum-id forum-id))
                           (xfactory:text description)))
                     (E :li
                        (estrong (substring title 64)))))

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
                              (when (admin-p (username))
                                (E :a
                                   (eclass "delete-this")
                                   (ehref (genurl 'delete-topic-message :message-id message-id))
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

(define-simple-route new-topic-message ("forum/thread/:(topic-id)"
                                        :method :post
                                        :login-status :logged-on)
  (let ((body (hunchentoot:post-parameter "body")))
    (unless (string= body "")
      (with-rulisp-db
        (insert-new-message topic-id
                            body
                            (username))))
      (hunchentoot:redirect (genurl 'view-topic :topic-id topic-id))))


(define-simple-route delete-topic-message ("forum/message/delete/:(message-id)"
                                           :login-status :logged-on)
  (if (admin-p (username))
      (with-rulisp-db
        (let ((topic-id (postmodern:query (format nil "SELECT * from rlf_delete_message(~A)" message-id) :single)))
          (hunchentoot:redirect (if (eql topic-id :null)
                                    (genurl 'forum-main)
                                    (genurl 'view-topic :topic-id topic-id)))))
      hunchentoot:+HTTP-FORBIDDEN+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rss-feed (description messages)
  (in-pool
   (xfactory:with-document-factory ((RSS))
     (RSS "rss"
          (xfactory:attributes "version" "2.0")
          (RSS "channel"
               (RSS "title"
                    (xfactory:text description))
               (RSS "link"
                    (xfactory:text "http://~A~A"
                                   (hunchentoot:host)
                                   (genurl 'forum-main)))
               (RSS "description"
                    (xfactory:text "~A - RSS-лента" description))

               (iter (for (forum-id topic-id author message date title) in messages)
                     (RSS "item"
                          (RSS "title"
                               (xfactory:text "~A: ~A" author title))
                          (RSS "link"
                               (xfactory:text "http://~A~A"
                                              (hunchentoot:host)
                                              (genurl 'view-topic :topic-id topic-id)))
                          (RSS "description"
                               (xfactory:text message))
                          (RSS "pubDate"
                               (xfactory:text date)))))))))

(define-simple-route all-forums-rss ("forum/rss/all.rss"
                                     :content-type "application/rss+xml")
  (with-rulisp-db
    (make-rss-feed (format nil "Форумы ~A" *host*)
                   (postmodern:query "SELECT pretty_forum_id, topic_id,  m.author, m.message,
                                             to_char(created, 'D, DD Mon YYYY HH24:MI:SS') || ' GMT+3' as date,
                                             title
                                       FROM rlf_messages AS m
                                       JOIN rlf_topics AS t USING (topic_id)
                                       JOIN rlf_forums AS f USING (forum_id)
                                       ORDER BY created DESC
                                       LIMIT 20"))))

(define-simple-route forum-rss ("forum/rss/:(forum-id).rss"
                                :content-type "application/rss+xml")
  
  (with-rulisp-db
    (make-rss-feed (postmodern:query (format nil
                                             "SELECT description FROM rlf_forums WHERE pretty_forum_id = '~A'"
                                             forum-id)
                                     :single)
                   (postmodern:query (format nil
                                             "SELECT forum_id, topic_id,  m.author, m.message,
                                                     to_char(created, 'D, DD Mon YYYY HH24:MI:SS') || ' GMT+3' as date,
                                                     title
                                              FROM rlf_messages AS m
                                              JOIN rlf_topics AS t USING (topic_id)
                                              JOIN rlf_forums AS f USING (forum_id)
                                              WHERE f.pretty_forum_id = '~A'
                                              ORDER BY created DESC
                                              LIMIT 20"
                                             forum-id)))))


(define-simple-route topic-rss ("forum/rss/threads/:(topic-id).rss"
                                :content-type "application/rss+xml")
  (with-rulisp-db
    (make-rss-feed (postmodern:query (format nil
                                             "SELECT title FROM rlf_topics WHERE topic_id = ~A"
                                             topic-id)
                                     :single)
                   (postmodern:query (format nil
                                             "SELECT forum_id, topic_id,  m.author, m.message,
                                                     to_char(created, 'D, DD Mon YYYY HH24:MI:SS') || ' GMT+3' as date,
                                                     title
                                              FROM rlf_messages AS m
                                              JOIN rlf_topics AS t USING (topic_id)
                                              JOIN rlf_forums AS f USING (forum_id)
                                              WHERE topic_id = ~A
                                              ORDER BY created DESC
                                              LIMIT 20"
                                             topic-id)))))
