;;; rss.lisp

(in-package :rulisp)

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
