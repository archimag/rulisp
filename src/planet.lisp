;;; planet.lisp

(restas:define-plugin :restas-rulisp-planet-plugin
  (:use :cl :iter :planet)
  (:basepath (asdf:component-pathname (asdf:find-system :planet-standalone))))

(in-package :restas-rulisp-planet-plugin)

(defplanet *planet* 
    :name "Sample PLANET implemented on Common Lisp"
    :alternate-href "http://lisp.catap.ru/planet/"
    :self-href "http://lisp.catap.ru/planet/atom.xml"
    :feeds-path #P"/etc/planet-feeds.lisp")

(define-filesystem-route resources "planet/:(file)" "resources/${file}")

(define-simple-route atom ("planet/atom.xml")
  (planet-syndicate-feed *planet*))

(defun eid (value)
  (xfactory:attributes :id value))

(defun eclass (value)
  (xfactory:attributes :class value))

(defun ehref (value)
  (xfactory:attributes :href value))

(define-simple-route main ("planet/"
                           :overlay-master rulisp-plugin::*master*)
  (gp:object-register
   (xfactory:with-document-factory ((xhtml "http://www.w3.org/1999/xhtml"))
     (xhtml "overlay"
            (xhtml :head
                   (xhtml :title "Sample PLANET implemented on Common Lisp")
                   (xhtml :link
                          (xfactory:attributes :href "/planet/planet.css" :rel "stylesheet" :type "text/css"))
                   (xhtml :link
                          (xfactory:attributes :rel "alternate"
                                               :href "/atom.xml"
                                               :title "Sample PLANET implemented on Common Lisp"
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
                                               (ehref "atom.xml")))
                                 (xhtml :div
                                        (eid "suggest")
                                        (xhtml :a
                                               (ehref "mailto:sample@sample.org")
                                               "Предложить блог"))
                                 (xhtml :h3 "Авторы")
                                 (xhtml :ul
                                        (eid "authors")
                                        (iter (for feed in (sort (planet-feeds *planet*)
                                                                 #'string<
                                                                 :key #'(lambda (f)
                                                                          (author-name (feed-author f)))))
                                              (xhtml :li
                                                     (xhtml :a
                                                            (ehref (author-uri (feed-author feed)))
                                                            (xfactory:text (author-name (feed-author feed))))))))
                          (xhtml :div
                                 (eid "planet-content")
                                 (iter (for entry in-child-nodes (xtree:root (planet-syndicate-feed *planet*)) with (:local-name "entry"))
                                       (xhtml :div
                                              (eclass "entry")
                                              (xhtml :div
                                                     (eclass "entry-title")
                                                     (xhtml :a
                                                            (ehref (xpath:find-string entry
                                                                                      "atom:link/@href"
                                                                                      :ns-map *feeds-ns-map*))
                                                            (xfactory:text (xpath:find-string entry
                                                                                              "atom:title"
                                                                                              :ns-map *feeds-ns-map*)))
                                                     (xhtml :div
                                                            (eclass "entry-author-info")
                                                            (xhtml :strong "Источинк: ")
                                                            (xhtml :a
                                                                   (ehref (xpath:find-string entry
                                                                                             "atom:author/atom:uri"
                                                                                             :ns-map *feeds-ns-map*))
                                                                   (xfactory:text (xpath:find-string entry
                                                                                                     "atom:author/atom:name"
                                                                                                     :ns-map *feeds-ns-map*)))))
                                              (xhtml :div
                                                     (eclass "entry-content")
                                                     (html:with-parse-html (doc (xpath:find-string entry
                                                                                                   "atom:content"
                                                                                                   :ns-map *feeds-ns-map*))
                                                       (iter (for node in-child-nodes (or (xpath:find-single-node (xtree:root doc) "body")
                                                                                          (xtree:root doc)))
                                                             (xtree:append-child xfactory::*node* (xtree:copy node))))))))))))))

