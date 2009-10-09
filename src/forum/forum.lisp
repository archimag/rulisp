;;; forum.lisp

(in-package :rulisp.forum)

(defun forum-admin-p (name)
  (find name '("archimag" "lispnik" "turtle") :test #'string=))


;;; main

(define-route forum-main ("")
  (with-rulisp-db
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
             (ecss 'rulisp::css :file "forum.css" :theme (user-theme (username))))
          (E :div
             (eid "content")
             (E :a
                (eclass "rss-link")
                (ehref 'all-forums-rss)
                "RSS")
             (iter (for (id description)
                        in (postmodern:query "SELECT pretty_forum_id, description FROM rlf_forums ORDER BY forum_id"))
                   (E :div
                      (eclass "forum")
                      (E :a
                         (ehref id)
                         (xfactory:text description))))))))))
