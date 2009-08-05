;;; topics.lisp

(in-package :rulisp)

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
                (last (min (+ 10 start) all-topics))
                (topics (select-topics forum-id start))
                (theme (user-theme (username))))
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
              (ecss 'css :file "forum.css" :theme theme)
              (ecss 'css :file  "jquery.wysiwyg.css" :theme theme)
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
                          (when (forum-admin-p (username))
                            (E :a
                               (eclass "delete-this")
                               (ehref 'delete-topic :topic-id topic-id)
                               "Удалить"))
                          (E :a
                             (ehref 'view-topic :topic-id topic-id)
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
  (redirect 'view-forum-main :forum-id forum-id))
  

(define-simple-route delete-topic ("forum/thread/delete/:(topic-id)"
                                   :login-status :logged-on)
  (if (forum-admin-p (username))
      (with-rulisp-db
        (let ((forum-id (postmodern:query (format nil "SELECT * from rlf_delete_topic(~A)" topic-id) :single)))
          (if (eql topic-id :null)
              (redirect 'forum-main)
              (redirect 'view-forum-main :forum-id forum-id))))
      hunchentoot:+HTTP-FORBIDDEN+))

