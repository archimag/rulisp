;;; planet.lisp


(in-package :rulisp)

(planet:defplanet *planet* 
    :name "Russian Lisp Planet"
    :alternate-href (format nil "http://~A/planet/" *host*)
    :self-href (format nil "http://~A/planet/atom.xml" *host*)
    :feeds-path #P"/etc/planet-feeds.lisp")

(defparameter *planet-path* (asdf:component-pathname (asdf:find-system :planet)))

(defun planet-path (path)
  (merge-pathnames path *planet-path*))

(define-simple-route planet-resources ("planet/:(file)")
  (declare (ignore file))
  (planet-path (restas:expand-text "resources/${file}" *bindings*)))
  

(define-simple-route planet-atom ("planet/atom.xml"
                                  :content-type "application/atom+xml")
  (planet:planet-syndicate-feed *planet*))

(define-simple-route planet-main ("planet/"
                           :overlay-master *master*)
  (in-pool
   (xfactory:with-document-factory ((xhtml "http://www.w3.org/1999/xhtml"))
     (xhtml "overlay"
            (xhtml :head
                   (xhtml :title "Russian Lisp Planet")
                   (xhtml :link
                          (xfactory:attributes :href "/planet/planet.css" :rel "stylesheet" :type "text/css"))
                   (xhtml :link
                          (xfactory:attributes :rel "alternate"
                                               :href (genurl 'planet-atom)
                                               :title "Russian Lisp Planet"
                                               :type "application/atom+xml")))
            (xhtml "div"
                   (eid "content")
                   (xhtml :div
                          (eid "planet-body")
                          (xhtml :div
                                 (eid "planet-info-panel")
                                 (xhtml :div
                                        (eid "syndicate")
                                        (xhtml :a
                                               "Подписаться"
                                               (ehref 'planet-atom)))
                                 (xhtml :div
                                        (eid "suggest")
                                        (xhtml :a
                                               (ehref "mailto:archimag@gmail.com")
                                               "Предложить блог"))
                                 (xhtml :h3 "Авторы")
                                 (xhtml :ul
                                        (eid "authors")
                                        (iter (for feed in (sort (copy-list (planet:planet-feeds *planet*))
                                                                 #'string<
                                                                 :key #'(lambda (f)
                                                                          (planet:author-name (planet:feed-author f)))))
                                              (xhtml :li
                                                     (xhtml :a
                                                            (ehref (planet:author-uri (planet:feed-author feed)))
                                                            (xfactory:text (planet:author-name (planet:feed-author feed))))))))
                          (xhtml :div
                                 (eid "planet-content")
                                 (iter (for entry in-child-nodes (xtree:root (planet:planet-syndicate-feed *planet*)) 
                                            with (:local-name "entry"))
                                       (xhtml :div
                                              (eclass "entry")
                                              (xhtml :div
                                                     (eclass "entry-title")
                                                     (xhtml :a
                                                            (ehref (xpath:find-string entry
                                                                                      "atom:link/@href"
                                                                                      :ns-map planet:*feeds-ns-map*))
                                                            (xfactory:text (xpath:find-string entry
                                                                                              "atom:title"
                                                                                              :ns-map planet:*feeds-ns-map*)))
                                                     (xhtml :div
                                                            (eclass "entry-author-info")
                                                            (xhtml :strong "Источник: ")
                                                            (xhtml :a
                                                                   (ehref (xpath:find-string entry
                                                                                             "atom:author/atom:uri"
                                                                                             :ns-map planet:*feeds-ns-map*))
                                                                   (xfactory:text (xpath:find-string entry
                                                                                                     "atom:author/atom:name"
                                                                                                     :ns-map planet:*feeds-ns-map*)))))
                                              (xhtml :div
                                                     (eclass "entry-content")
                                                     (html:with-parse-html (doc (xpath:find-string entry
                                                                                                   "atom:content"
                                                                                                   :ns-map planet:*feeds-ns-map*))
                                                       (iter (for node in-child-nodes (or (xpath:find-single-node (xtree:root doc) "body")
                                                                                          (xtree:root doc)))
                                                             (xtree:append-child xfactory::*node* (xtree:copy node))))))))))))))

