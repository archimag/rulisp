;;; wiki.lisp

(in-package :rulisp)

(defun wiki-page-pathname (page)
  (merge-pathnames (format nil "pages/~A" (hunchentoot:url-encode  page))
                   *wiki-dir*))

(defun wiki-page-changes-pathname (page)
  (merge-pathnames (format nil "changes/~A.changes" (hunchentoot:url-encode  page))
                   *wiki-dir*))

(defun wiki-page-archive-pathname (page time)
  (merge-pathnames (format nil "archive/~A.~A.gz" (hunchentoot:url-encode page) time)
                   *wiki-dir*))
  

(defun wiki-page-menu (page)
  (xfactory:with-element-factory ((E))
    (E :div
       (eclass "wiki-page-menu")
       (E :ul
          (E :li
             (E :a
                (ehref 'edit-wiki-page :page (hunchentoot:url-decode page))
                "Править"))
          (E :li
             (E :a
                (ehref 'history-wiki-page :page (hunchentoot:url-decode page))
                "История"))))))
  

(defun show-wiki-page (page)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :overlay
        (E :head
           (E :title
              (etext (hunchentoot:url-decode page)))
           (ecss 'css :file "wiki.css" :theme (user-theme (username))))
        (E :div
           (eid "content")                 
           (let ((path (wiki-page-pathname page)))
             (if (fad:file-exists-p path)
                 (progn
                   (when (username)
                     (wiki-page-menu page))
                   (render-wiki-page (wiki-parser:parse :dokuwiki
                                                        path)))
                 (progn
                   (E :h1
                      "Эта страница ещё не существует")
                   (E :a
                      (ehref 'edit-wiki-page :page (hunchentoot:url-decode page))
                      "Создать")))))))))
                   
(define-simple-route wiki-main-page ("wiki/"
                                     :overlay-master *master*)
  (show-wiki-page "index"))


(define-simple-route view-wiki-page ("wiki/:(page)"
                                     :overlay-master *master*)
  (show-wiki-page page))

(define-simple-route edit-wiki-page ("wiki/edit/:(page)"
                                     :overlay-master *master*
                                     :login-status :logged-on)
  (let ((doc (in-pool (xtree:parse (expand-file (tmplpath "wiki/edit.xml")
                                                `((:title . ,(hunchentoot:url-decode page))))))))
    (if (fad:file-exists-p (wiki-page-pathname page))        
        (fill-form doc (acons "page-content"
                              (alexandria:read-file-into-string (wiki-page-pathname page))
                              nil)))
    doc))


(defun save-wiki-page (page content author &optional comment)
  (let* ((time (get-universal-time))
         (page-path (ensure-directories-exist (wiki-page-pathname page)))
         (changes-path (ensure-directories-exist (wiki-page-changes-pathname page)))
         (archive-path (ensure-directories-exist (wiki-page-archive-pathname page time)))
         (changes (nconc (if (fad:file-exists-p changes-path)
                          (with-open-file (in changes-path)
                            (with-standard-io-syntax
                              (read in))))
                      (list (list time
                                  author
                                  (if (fad:file-exists-p page-path)
                                      :edit
                                      :create)
                                  (hunchentoot:url-decode page)
                                  comment)))))
    (with-open-file (out changes-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (print changes
               out)))
    (write-string-into-gzip-file content archive-path)
    (alexandria:write-string-into-file content
                                       page-path
                                       :if-exists :supersede
                                       :if-does-not-exist :create)))

(define-simple-route edit-wiki-page/post ("wiki/edit/:(page)"
                                     :overlay-master *master*
                                     :method :post
                                     :login-status :logged-on)
  (cond
    ((hunchentoot:post-parameter "cancel") (redirect 'view-wiki-page 
                                                     :page page))    
    ((hunchentoot:post-parameter "preview") (let* ((page-content (hunchentoot:post-parameter "page-content"))
                                                   (doc (in-pool (xtree:parse (expand-file (tmplpath "wiki/edit.xml")
                                                                                          `((:title . ,page))))))
                                                   (xfactory:*node* (xpath:find-single-node doc "//*[@id='content']")))
                                              (fill-form doc (acons "page-content"
                                                                    page-content
                                                                    nil))                                              
                                              (render-wiki-page (wiki-parser:parse :dokuwiki
                                                                                   page-content))
                                              doc))
    (t (progn
         (save-wiki-page page
                         (hunchentoot:post-parameter "page-content")
                         (username))
         (redirect 'view-wiki-page
                   :page page)))))
                                     


(define-simple-route history-wiki-page ("wiki/history/:(page)"
                                        :overlay-master *master*
                                        :login-status :logged-on)
  (let* ((change-path (wiki-page-changes-pathname page))
         (changes (nreverse (if (fad:file-exists-p change-path)
                                (with-open-file (in change-path)
                                  (with-standard-io-syntax
                                    (read in))))))
         (time-format '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\Space (:HOUR 2) #\: (:MIN 2))))
    (in-pool
     (xfactory:with-document-factory ((E))
       (E :overlay
          (E :head
             (E :title
                (etext "История: ~A" (hunchentoot:url-decode page)))
             (ecss 'css :file "wiki.css" :theme (user-theme (username))))
          (E :div
             (eid "content")
             (E :div
                (eclass "wiki-page-menu")
                (E :ul
                   (E :li
                      (E :a
                         (ehref 'view-wiki-page :page (hunchentoot:url-decode page))
                         "Смотреть"))))
             (E :h1 "История страницы")
             (E :ul
                (iter (for item in changes)
                      (E :li
                         (etext "~A "
                                (local-time:format-timestring nil
                                                              (local-time:universal-to-timestamp (first item))
                                                              :format time-format))
                         (E :a
                            (ehref 'view-archive-wiki-page
                                   :page (hunchentoot:url-decode page)
                                   :time (first item))
                            (etext (fourth item)))
                         (E :spane
                            (estyle "color: #666")
                            (etext " ~A" (second item))))))))))))

(define-simple-route view-archive-wiki-page ("wiki/history/:(page)/:(time)"
                                             :overlay-master *master*)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :overlay
        (E :head
           (E :title
              (etext (hunchentoot:url-decode page)))
           (ecss 'css :file "wiki.css" :theme (user-theme (username))))
        (E :div
           (eid "content")
           (E :div
              (eclass "wiki-page-menu")
              (E :ul
                 (E :li
                    (E :a
                       (ehref 'view-wiki-page :page (hunchentoot:url-decode page))
                       "Текущая версия"))
                 (E :li
                    (E :a
                       (ehref 'history-wiki-page :page (hunchentoot:url-decode page))
                       "История"))
                    ))
           (let ((path (wiki-page-archive-pathname page time)))
             (if (fad:file-exists-p path)
                 (render-wiki-page (wiki-parser:parse :dokuwiki (read-gzip-file-into-string path)))
                 "Архивный файл не обнаружен")))))))