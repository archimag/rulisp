;;; rss.lisp

(in-package :rulisp.forum)

(defun make-rss-feed (description messages)
  (in-pool
   (xfactory:with-document-factory ((RSS))
     (RSS "rss"
          (xfactory:attributes "version" "2.0")
          (RSS "channel"
               (RSS "title"
                    (xfactory:text description))
               (RSS "link"
                    (xfactory:text (genurl-with-host 'forum-main)))
               (RSS "description"
                    (xfactory:text "~A - RSS-лента" description))

               (iter (for (forum-id topic-id author message date title) in messages)
                     (RSS "item"
                          (RSS "title"
                               (xfactory:text "~A: ~A" author title))
                          (RSS "link"
                               (xfactory:text (genurl-with-host 'view-topic :topic-id topic-id)))
                          (RSS "description"
                               (xfactory:text message))
                          (RSS "pubDate"
                               (xfactory:text (local-time:format-http-timestring nil (local-time:universal-to-timestamp date))
                                              )))))))))

(define-simple-route all-forums-rss ("rss/all.rss"
                                     :content-type "application/rss+xml")
  (with-rulisp-db
    (make-rss-feed (format nil "Форумы ~A" *host*)
                   (postmodern:query "SELECT pretty_forum_id, topic_id,  m.author, m.message,
                                             created AT TIME ZONE 'GMT',
                                             title
                                       FROM rlf_messages AS m
                                       JOIN rlf_topics AS t USING (topic_id)
                                       JOIN rlf_forums AS f USING (forum_id)
                                       ORDER BY created DESC
                                       LIMIT 20"))))

(define-simple-route forum-rss ("rss/:(forum-id).rss"
                                :content-type "application/rss+xml")
  
  (with-rulisp-db
    (make-rss-feed (postmodern:query (format nil
                                             "SELECT description FROM rlf_forums WHERE pretty_forum_id = '~A'"
                                             forum-id)
                                     :single)
                   (postmodern:query (format nil
                                             "SELECT forum_id, topic_id,  m.author, m.message,
                                                     created AT TIME ZONE 'GMT',
                                                     title
                                              FROM rlf_messages AS m
                                              JOIN rlf_topics AS t USING (topic_id)
                                              JOIN rlf_forums AS f USING (forum_id)
                                              WHERE f.pretty_forum_id = '~A'
                                              ORDER BY created DESC
                                              LIMIT 20"
                                             forum-id)))))


(define-simple-route topic-rss ("rss/threads/:(topic-id).rss"
                                :content-type "application/rss+xml")
  (with-rulisp-db
    (make-rss-feed (postmodern:query (format nil
                                             "SELECT title FROM rlf_topics WHERE topic_id = ~A"
                                             topic-id)
                                     :single)
                   (postmodern:query (format nil
                                             "SELECT forum_id, topic_id,  m.author, m.message,
                                                     created AT TIME ZONE 'GMT',
                                                     title
                                              FROM rlf_messages AS m
                                              JOIN rlf_topics AS t USING (topic_id)
                                              JOIN rlf_forums AS f USING (forum_id)
                                              WHERE topic_id = ~A
                                              ORDER BY created DESC
                                              LIMIT 20"
                                             topic-id)))))
