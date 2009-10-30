;;; format.lisp

(in-package :rulisp.format)

(defun formater-menu ()
  (xfactory:with-element-factory ((E))
    (E :div
       (eid "second-menu")
       (E :ul
          (E :li
             (E :a
                (ehref 'format-main)
                "Все записи"))
          (E :li
             (E :a
                (ehref 'newformat)
                "Создать"))))))

(define-route chrome-formater-menu ("formater/topmenu"
                                           :protocol :chrome)
  (xtree:with-object (el (formater-menu))
    (xtree:serialize el :to-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-nav-panel (start end all &key (step 20))
  (let ((url (genurl 'format-main)))
    (xfactory:with-element-factory ((E))
      (E :div
         (xfactory:attributes :class "item-nav-panel"
                              :align "right")
         (E :span
            (estrong "~A" (1+ start))         
            " - "
            (estrong "~A" (min end all))
            " из "
            (estrong "~A" all)
            " « "
            (if (> start 0)
                (E :a
                   (if (> start step)
                       (ehref "~A?start=~A" url (- start step))
                       (ehref url))
                   "Позже")
                (xfactory:text "Позже"))
            " | "
            (if (< (+ start step) all)
                (E :a
                   (ehref "~A?start=~A" url (+ start step))
                   "Раньше")
                (xfactory:text "Раньше"))
            " » ")))))

(postmodern:defprepared select-formats*
  "SELECT f.format_id, u.login, f.title, to_char(f.created, 'DD.MM.YYYY HH24:MI') FROM formats AS f
    LEFT JOIN users AS u USING (user_id)
    ORDER BY f.created DESC
    LIMIT $2 OFFSET $1")

(defun select-formats (start &optional (limit 10))
  (select-formats* start limit))


(define-route format-main ("all")
  (let* ((start* (hunchentoot:get-parameter "start"))
         (start (if start*
                    (parse-integer start*)
                    0))
         (items (rulisp:with-rulisp-db
                  (select-formats start)))
         (all (rulisp:with-rulisp-db
                (postmodern:query (:select (:count '*) :from 'formats) :single))))
    (in-pool
     (xfactory:with-document-factory ((E))
       (E :overlay
          (E :div
             (eid "content")
             (formater-menu)
             (format-nav-panel start (+ start (length items)) all :step 10)
             (iter (for (format-id author title created) in items)
                   (E :div
                      (eclass "item")
                      (E :a
                         (ehref 'view-format-code :format-id format-id)
                         (xfactory:text (if (string= title "")
                                            "*notitle*"
                                            title)))
                      (E :div
                         (eclass "info")
                         (E :span
                            (eclass "info")
                            "Автор: "
                            (estrong "~A" author)
                            (xfactory:text " - ~A" created)))))))))))
                       


(define-route newformat ("")
  (in-pool
   (let ((doc (xtree:parse (rulisp:tmplpath "format.xml"))))
     (xtree:with-custom-resolvers ((lambda (url id ctxt)
                                     (declare (ignore id))
                                     (if (and (eql (puri:uri-scheme url) :chrome)
                                              (string= (concatenate 'string
                                                                           (puri:uri-host url)
                                                                           (puri:uri-path url))
                                                       "formater/topmenu"))
                                            (xtree:with-object (menu (formater-menu))
                                              (xtree:resolve-string  (print (xtree:serialize menu :to-string)) ctxt))
                                         )))
       (xtree:process-xinclude doc)
       doc))))

(postmodern:defprepared db-new-format-code "SELECT * FROM add_format_code($1, $2, $3)" :single)
  
(define-route newformat/post (""
                                    :method :post)
  (if (hunchentoot:post-parameter "preview")
      (let* ((doc (in-pool (xtree:parse (rulisp:tmplpath "format.xml"))))
             (form (xpath:find-single-node doc "//form")))
        (rulisp:fill-form doc (hunchentoot:post-parameters*))
        (let ((xfactory:*node* (xtree:insert-child-before (xtree:make-element "div")  form)))
          (xfactory:with-element-factory ((E))
            (E :div
               (eclass "preview")
               (E :h3
                  "Предварительный просмотр")
               (E :pre
                  (eclass "code")
                  (e-text2html (rulisp:code-to-html (hunchentoot:post-parameter "code")))))))
        (when (username)
          (let ((xfactory:*node* form))
            (xfactory:with-element-factory ((E))
              (E :div
                 (eclass "format-save")
                 (estrong "Описание: ")
                 (E :input
                    (xfactory:attributes :type "text"
                                         :name "title"
                                         :size 60))
                 (e-break-line)
                 (e-break-line)
                 (E :input
                    (xfactory:attributes :type "submit"
                                         :value "Сохранить"))))))
        doc)
      (if (username)
          (let ((title (hunchentoot:post-parameter "title"))
                (code (hunchentoot:post-parameter "code")))
            (if (and code
                     (not (string= code "")))
                (restas:redirect 'view-format-code
                          :format-id (rulisp:with-rulisp-db
                                       (db-new-format-code (username) title code)))
                (in-pool (xtree:parse (rulisp:tmplpath "format.xml")))))
          hunchentoot:+HTTP-FORBIDDEN+)))


(postmodern:defprepared get-format-code
    "SELECT u.login, f.title, f.code, to_char(f.created, 'DD.MM.YYYY HH24:MI') FROM formats AS f
     LEFT JOIN users AS u USING (user_id)
     WHERE format_id = $1"
  :row)

(define-route view-format-code (":(format-id)")
  (let ((row (rulisp:with-rulisp-db (get-format-code format-id))))
    (if row
        (in-pool
         (xfactory:with-document-factory ((E))
           (E :overlay
              (E :head
                 (E :title
                    (xfactory:text "Форматтер: ~A"
                                   (let ((title (second row)))
                                     (if (string= title "")
                                         "Безымянный код"
                                         title)))))
              (E :div
                 (eid "content")
                 (formater-menu)
                 (estrong (second row))
                 (e-break-line)
                 (E :span
                    (eclass "info")
                    "Автор: "
                    (estrong "~A"  (first row))
                    (xfactory:text " - ~A" (fourth row)))
                 (E :pre
                    (eclass "code")
                    (e-text2html (rulisp:code-to-html (third row))))))))
        hunchentoot:+HTTP-NOT-FOUND+)))