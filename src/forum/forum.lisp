;;; forum.lisp

(in-package :rulisp)

(defun forum-admin-p (name)
  (string= name "archimag"))

;;; main

(define-simple-route forum-main ("forum/"
                                 :overlay-master *master*)
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
             (ecss "/css/forum.css"))
          (E :div
             (eid "content")
             (iter (for (id description) in (postmodern:query "SELECT pretty_forum_id, description FROM rlf_forums"))
                   (E :div
                      (eclass "forum")
                      (E :a
                         (ehref id)
                         (xfactory:text description))))))))))
