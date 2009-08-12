;;; pcl.lisp

(in-package :rulisp)

(defparameter *test-pcl-page-path*
  (merge-pathnames "content/macros-standard-control-constructs.dokuwiki" *basepath*))

(defparameter *test-pcl-page*  (wiki-parser:parse :dokuwiki *test-pcl-page-path*))


(defparameter *wiki-render-map* (make-hash-table))

(defun todo-dokuwiki ()
  (set-difference (iter (for (key value) in-hashtable dokuwiki::*symbols-category*)
                        (collect key))
                  (iter (for (key value) in-hashtable *wiki-render-map*)
                        (collect key))))

(defun render-wiki-item (item)
  (cond
    ((and (consp item)
          (symbolp (car item))) (let ((render (gethash (car item) *wiki-render-map*)))
                                  (if render
                                      (funcall render (cdr item))
                                      (render-wiki-item (cdr item)))))
    ((consp item) (iter (for i in item)
                        (render-wiki-item i)))
    ((symbolp item) (let ((render (gethash item *wiki-render-map*)))
                      (if render
                          (funcall render nil))))
    ((stringp item) (xfactory:text item))))

(defun render-wiki-page (wikidoc)
  (xfactory:with-element-factory ((E))
    (E :div
       (estyle "margin-bottom: 1em")
       (render-wiki-item wikidoc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-all-wiki-items (items)
  (iter (for item in items)
        (render-wiki-item item)))


(defmacro define-wiki-render (name (items) &body body)
  `(setf (gethash ',name
                  *wiki-render-map*)
         (lambda (,items)
           ,@body)))

(define-wiki-render dokuwiki:toplevel (items)
  (render-all-wiki-items items))
  
(define-wiki-render dokuwiki:chapter (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node*
                                                   "div")))
    (eclass "chapter")
    (render-all-wiki-items items)))


(define-wiki-render dokuwiki:header (items)
  (xtree:make-child-text (xtree:make-child-element xfactory:*node*
                                                   "h3")
                         (car items)))

(defparameter +endl+ (string #\Newline))

(define-wiki-render dokuwiki:eol (items)
  (declare (ignore items))
  (xfactory:text +endl+))


(define-wiki-render dokuwiki:paragraph (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node*
                                                   "p")))
    (render-all-wiki-items items)))


(define-wiki-render dokuwiki:footnote (items)
  (declare (ignore items))
  ;;(estrong "'footnote - fix me'")
  )

(define-wiki-render dokuwiki:linebreak (items)
  (declare (ignore items))
  (xtree:make-child-element xfactory:*node* "br"))

(define-wiki-render dokuwiki:monospace (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "code")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:strong (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "strong")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:emphasis (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "em")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:underline (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "u")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:preformatted (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "pre")))
    (iter (for item in items)pp
          (render-wiki-item item)
          (e-break-line))))
  
(define-wiki-render dokuwiki:code (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "pre")))
    (eclass "code")
    (e-text2html (code-to-html (car items)))))

(define-wiki-render dokuwiki:quoted (items)
  (let ((xfactory:*node* (xtree:make-child-element xfactory:*node* "blockquote")))
    (render-all-wiki-items items)))

(define-wiki-render dokuwiki:unformatted (items)
  (iter (for item in (alexandria:flatten items))
        (cond
          ((stringp item) (xfactory:text item))
          ((eql item 'dokuwiki:eol) (xfactory:text +endl+)))))

(define-wiki-render dokuwiki:unformattedalt (items)
  (iter (for item in (alexandria:flatten items))
        (cond
          ((stringp item) (xfactory:text item))
          ((eql item 'dokuwiki:eol) (xfactory:text +endl+)))))

(define-wiki-render dokuwiki:html (items)
  (xfactory:with-element-factory ((E))
    (E :pre
       (iter (for item in (alexandria:flatten items))
        (cond
          ((stringp item) (xfactory:text item))
          ((eql item 'dokuwiki:eol) (xfactory:text +endl+)))))))
       

;;(define-wiki-render dokuwiki:html (items)

(define-wiki-render dokuwiki:hr (items)
  (declare (ignore items))
  (xtree:make-child-element xfactory:*node* "hr"))

(define-wiki-render dokuwiki:unordered-listblock (items)
  (xfactory:with-element-factory ((E))
    (E :ul
       (iter (for item in items)
             (E :li
                (render-wiki-item item))))))

(define-wiki-render dokuwiki:ordered-listblock (items)
  (xfactory:with-element-factory ((E))
    (E :ol
       (iter (for item in items)
             (E :li
                (render-wiki-item item))))))

;; (define-wiki-render dokuwiki:table (items)
;;   (xfactory:with-element-factory ((E))
;;     (E :pre
;;        (xfactory:text (write-to-string items)))))

(define-wiki-render dokuwiki:external-link (items)
  (let ((delimiter (position #\| (car items))))
    (xfactory:with-element-factory ((E))
      (E :a
         (ehref (string-trim '#(#\Space #\Tab)
                             (if delimiter
                                 (subseq (car items) 0 delimiter)
                                 (car items))))
         (xfactory:text (string-trim '#(#\Space #\Tab)
                                     (if delimiter
                                         (subseq (car items) (1+ delimiter))
                                         (car items))))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pcl-files-map*
  '#(("introduction-why-lisp"
      "Введение: почему Lisp?" 
      "%D0%B2%D0%B2%D0%B5%D0%B4%D0%B5%D0%BD%D0%B8%D0%B5%D0%BF%D0%BE%D1%87%D0%B5%D0%BC%D1%83lisp")
    
     ("lather-rinse-repeat-a-tour-of-the-repl"
      "Намылить, смыть, повторить: знакомство с REPL"
      "%D1%82%D1%83%D1%80%D0%B2repl")
    
     ("practical-a-simple-database"
      "Практикум: Простая база данных"
      "%D0%BF%D1%80%D0%B0%D0%BA%D1%82%D0%B8%D0%BA%D1%83%D0%BC%D0%BF%D1%80%D0%BE%D1%81%D1%82%D0%B0%D1%8F%D0%B1%D0%B0%D0%B7%D0%B0%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85")
    
     ("syntax-and-semantics"
      "Синтаксис и семантика"
      "%D1%81%D0%B8%D0%BD%D1%82%D0%B0%D0%BA%D1%81%D0%B8%D1%81%D0%B8%D1%81%D0%B5%D0%BC%D0%B0%D0%BD%D1%82%D0%B8%D0%BA%D0%B0")
    
     ("functions"
      "Функции"
      "%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%B8")
    
     ("variables"
      "Переменные"
      "%D0%BF%D0%B5%D1%80%D0%B5%D0%BC%D0%B5%D0%BD%D0%BD%D1%8B%D0%B5")
    
     ("macros-standard-control-constructs"
      "Макросы: Стандартные управляющие конструкции"
      "%D0%BC%D0%B0%D0%BA%D1%80%D0%BE%D1%81%D1%8B%D1%81%D1%82%D0%B0%D0%BD%D0%B4%D0%B0%D1%80%D1%82%D0%BD%D1%8B%D0%B5%D1%83%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B8%D0%B5%D0%BA%D0%BE%D0%BD%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%86%D0%B8%D0%B8")
    
     ("macros-defining-your-own"
      "Макросы: Создание собственных макросов"
      "%D0%BC%D0%B0%D0%BA%D1%80%D0%BE%D1%81%D1%8B%D1%81%D0%BE%D0%B7%D0%B4%D0%B0%D0%BD%D0%B8%D0%B5%D1%81%D0%BE%D0%B1%D1%81%D1%82%D0%B2%D0%B5%D0%BD%D0%BD%D1%8B%D1%85%D0%BC%D0%B0%D0%BA%D1%80%D0%BE%D1%81%D0%BE%D0%B2")
    
     ("practical-building-a-unit-test-framework"
      "Практикум: Каркас для юнит-тестирования"
      "%D0%BF%D1%80%D0%B0%D0%BA%D1%82%D0%B8%D0%BA%D1%83%D0%BC%D0%BA%D0%B0%D1%80%D0%BA%D0%B0%D1%81%D0%B4%D0%BB%D1%8F%D1%8E%D0%BD%D0%B8%D1%82%D1%82%D0%B5%D1%81%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F")
    
     ("numbers-characters-and-strings"
      "Числа, знаки и строки"
      "%D1%87%D0%B8%D1%81%D0%BB%D0%B0%D1%81%D0%B8%D0%BC%D0%B2%D0%BE%D0%BB%D1%8B%D1%81%D1%82%D1%80%D0%BE%D0%BA%D0%B8")
    
     ("collections"
      "Коллекции"
      "%D0%BA%D0%BE%D0%BB%D0%BB%D0%B5%D0%BA%D1%86%D0%B8%D0%B8")
    
     ("they-called-it-lisp-for-a-reason-list-processing"
      "Он называется Lisp неспроста: обработка списков"
      "%D0%BE%D0%BD%D0%BD%D0%B0%D0%B7%D1%8B%D0%B2%D0%B0%D0%B5%D1%82%D1%81%D1%8Flisp%D0%BD%D0%B5%D1%81%D0%BF%D1%80%D0%BE%D1%81%D1%82%D0%B0%D0%BE%D0%B1%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B0%D1%81%D0%BF%D0%B8%D1%81%D0%BA%D0%BE%D0%B2")
    
     ("beyond-lists-other-uses-for-cons-cells"
      "Не только списки: Другие применения cons-ячеек"
      "%D0%BD%D0%B5%D1%82%D0%BE%D0%BB%D1%8C%D0%BA%D0%BE%D1%81%D0%BF%D0%B8%D1%81%D0%BA%D0%B8")
    
     ("files-and-file-io"
      "Файлы и файловый ввод/вывод"
      "%D1%84%D0%B0%D0%B9%D0%BB%D1%8B%D1%84%D0%B0%D0%B9%D0%BB%D0%BE%D0%B2%D1%8B%D0%B9%D0%B2%D0%B2%D0%BE%D0%B4%D0%B2%D1%8B%D0%B2%D0%BE%D0%B4")
    
     ("practical-a-portable-pathname-library"
      "Практика. Переносимая библиотека файловых путей"
      "practicalportablepathlib")
    
     ("object-reorientation-generic-functions"
      "ООП: Обобщенные функции"
      "oopgenericfunctions")
    
     ("object-reorientation-classes"
      "ООП: Классы"
      "oopclasses")
    
     ("a-few-format-recipes"
      "Несколько рецептов для функции FORMAT"
      "%D0%BD%D0%B5%D1%81%D0%BA%D0%BE%D0%BB%D1%8C%D0%BA%D0%BE%D1%80%D0%B5%D1%86%D0%B5%D0%BF%D1%82%D0%BE%D0%B2%D0%B4%D0%BB%D1%8Fformat")
    
     ("beyond-exception-handling-conditions-and-restarts"
      "Обработка исключений изнутри: Условия и Перезапуск"
      "%D0%BE%D0%B1%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B0%D0%B8%D1%81%D0%BA%D0%BB%D1%8E%D1%87%D0%B5%D0%BD%D0%B8%D0%B9")
    
     ("the-special-operators"
      "Специальные операторы"
      "%D1%81%D0%BF%D0%B5%D1%86%D0%B8%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80%D1%8B")
    
     ("programming-in-the-large-packages-and-symbols"
      "Программирование по-взрослому: Пакеты и Символы"
      "%D0%BF%D0%B0%D0%BA%D0%B5%D1%82%D1%8B%D0%B8%D1%81%D0%B8%D0%BC%D0%B2%D0%BE%D0%BB%D1%8B")
    
     ("loop-for-black-belts" "LOOP для мастеров с чёрным поясом" "loopforblackbelts")
    
     ("practical-a-spam-filter" "Практика. Спам-фильтр" "practicespamfilter")
    
     ("practical-parsing-binary-files" "Практика. Разбор двоичных файлов" "practiceparsingbinfiles")
    
     ("practical-an-id3-parser" "Практика. Разбор ID3" "practiceanid3parser")
    
     ("practical-web-programming-with-allegroserve" "Практика. Web-программирование с помощью AllegroServe" "practicewebprogramming")
    
     ("practical-an-mp3-database" "Практика. База данных для MP3" "mp3database")
    
     ("practical-a-shoutcast-server" "Практика. Сервер Shoutcast" "shoutcastserver")
    
     ("practical-an-mp3-browser" "Практика. Браузер MP3 файлов" "practicemp3browser")
    
     ("practical-an-html-generation-library-the-interpreter"
      "Практика: Библиотека для генерации HTML, Интерпретатор."
      "practicehtmlgenlibinterpreter")
    
     ("practical-an-html-generation-library-the-compiler"
      "Практика: Библиотека для генерации HTML, Компилятор."
      "practicehtmlgenlibcompiler")
    
     ("conclusion-whats-next" "Заключение: Что дальше ?" "conclusion")))

                               
(defun pcl-source-path (chapter)
  (merge-pathnames (concatenate 'string chapter ".txt")
                   *pcl-dir*))

(define-simple-route pcl-main ("pcl/"
                               :overlay-master *master*)
  (in-pool
   (xfactory:with-document-factory ((E))
     (E :overlay
        (E :head
           (E :title "Перевод Practical Common Lisp"))
        (E :div
           (eid "content")
           (E :p
              "Это перевод на русский язык замечательной книги "
              (E :a (ehref "http://www.gigamonkeys.com/book/") "Practical Common Lisp")
              ". Основная работа над переводом ведётся "
              (E :a (ehref "http://pcl.catap.ru/") "здесь")
              "."
              (e-break-line)
              (estrong "ОСТОРОЖНО!")
              " Этот сервис основан на ещё не отлаженном коде по парсингу и отображению "
              (E :a (ehref "http://www.dokuwiki.org/ru:dokuwiki") "dokuwiki")
              "-страниц,
 если Вы хотите быть уверены в точности отображения содержимого - обратитесь к "
              (E :a (ehref "http://pcl.catap.ru/") "источнику")
              " перевода")
           (E :ol
              (iter (for chapter in-vector *pcl-files-map*)
                    (E :li
                       (E :a
                          (ehref 'pcl-chapter-view :chapter (first chapter))
                          (xfactory:text (second chapter)))))))))))

(defun pcl-navigation-bar (number)
  (xfactory:with-element-factory ((E))
    (E :table
       (xfactory:attributes :width "100%")
       (E :tbody
          (E :tr
             (E :td
                (xfactory:attributes :width "20%"
                                     :align "left")
                (when (> number 0)
                  (E :a
                     (ehref 'pcl-chapter-view
                            :chapter (first (aref *pcl-files-map*
                                                  (1- number))))
                     "Предыдущая")))
             (E :td 
                (xfactory:attributes :width "60%"
                                     :align "center")
                (E :a
                   (ehref 'pcl-main)
                   "Оглавление"))
             (E :td
                (xfactory:attributes :width "20%"
                                     :align "right")
                (when (< number (1- (length *pcl-files-map*)))
                  (E :a
                     (ehref 'pcl-chapter-view
                            :chapter (first (aref *pcl-files-map*
                                                  (1+ number))))
                     "Следующая"))))))))

(define-simple-route pcl-chapter-view ("pcl/:(chapter)"
                                       :overlay-master *master*)
  (let* ((number (position chapter
                           *pcl-files-map*
                           :key #'first
                           :test #'string=))
         (path (pcl-source-path (third (aref *pcl-files-map* number)))))
    (if (fad:file-exists-p path)
        (in-pool
         (xfactory:with-document-factory ((E))
           (E :overlay
              (E :head
                 (E :title
                    (xfactory:text "~A" (second (aref *pcl-files-map* number)))))
              (E :div
                 (eid "content")
                 (pcl-navigation-bar number)
                 (render-wiki-page (wiki-parser:parse :dokuwiki
                                                      path))
                 (pcl-navigation-bar number)))))
        hunchentoot:+HTTP-NOT-FOUND+)))

