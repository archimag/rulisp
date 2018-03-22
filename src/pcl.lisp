;;;; pcl.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :rulisp.pcl)

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

                               
;; (defun pcl-source-path (chapter)
;;   (merge-pathnames (concatenate 'string chapter ".txt")
;;                    *pcl-dir*))

(defun pcl-source-path (chapter)
  (merge-pathnames (format nil "chapter-~2,'0d.txt" chapter)
                   *pcl-dir*))

(defun finalize-page (content title)
  (rulisp::rulisp-finalize-page :title title
                                :css '("style.css" "colorize.css")
                                :content content))

(defun pcl-cover ()
  (merge-pathnames "static/image/pcl.jpg"
                   rulisp::*resources-dir*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route pcl-main ("")
  (finalize-page (rulisp.view:pcl-main (list :pdf-href (restas:genurl 'pcl-pdf)
                                             :jpg-href "/image/pcl.jpg"
                                             :chapters (iter (for chapter in-vector *pcl-files-map*)
                                                             (collect (list :href (restas:genurl 'pcl-chapter-view
                                                                                                 :chapter (first chapter))
                                                                            :title (second chapter))))))
                 "Перевод Practical Common Lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chapter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(defun chapter-url (number)
  (if (and (> number -1)
           (< number (1- (length *pcl-files-map*))))
      (restas:genurl 'pcl-chapter-view
                     :chapter (first (aref *pcl-files-map*
                                           number)))))

(restas:define-route pcl-chapter-view (":(chapter)")
  (let* ((number (position chapter
                           *pcl-files-map*
                           :key #'first
                           :test #'string=))
         (path (pcl-source-path (1+ number))))
    (if (fad:file-exists-p path)
        (finalize-page (rulisp.view:pcl-chapter-view (list :prev (chapter-url (1- number))
                                                           :menu (restas:genurl 'pcl-main)
                                                           :next (chapter-url (1+ number))
                                                           :content (xtree:with-object (el (rulisp::render-wiki-page
                                                                                            (wiki-parser:parse :dokuwiki path)))
                                                                      (xtree:serialize el :to-string))))
                       (second (aref *pcl-files-map* number)))
        hunchentoot:+HTTP-NOT-FOUND+)))


(restas:define-route pcl-chapter-pdf ("pdf/:(chapter)"
                                      :content-type "application/pdf")
  (let* ((number (position chapter
                           *pcl-files-map*
                           :key #'first
                           :test #'string=))
         (path (pcl-source-path (third (aref *pcl-files-map* number)))))
    (flexi-streams:with-output-to-sequence (out)
      (let ((out* (flexi-streams:make-flexi-stream out)))
        (rulisp::pdf-render-wiki-page (wiki-parser:parse :dokuwiki
                                                         path)
                                      out*))
      out)))


(defun pcl-first-page ()
  (let ((result))
    (pdf:with-page ()
      (setf result pdf:*page*)
      ;;(pdf:draw-centered-text 300 500 "Practical Common Lisp" *header-font* 30)
      (let ((bounds (pdf::bounds pdf:*page*))
            (image (pdf:make-image (merge-pathnames "static/image/pcl.jpg"
                                                    rulisp::*resources-dir*))))
        (pdf:add-images-to-page image)
        (pdf:draw-image image
                        0 0 (aref bounds 2) (aref bounds 3) 0))
      )
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pcl-pdf (&optional (out #P"/tmp/pcl.pdf"))
  (let ((page-number 1))
    (tt:with-document (:mode :outlines)
      (rulisp::append-child-outline (pdf::outline-root pdf:*document*) 
                                "Practical Common Lisp"
                                (let ((pdf:*page* (pcl-first-page)))
                                  (pdf:register-page-reference "Practical Common Lisp")))
      (let ((rulisp::*current-chapter* "Practical Common Lisp"))
        (iter (for chapter in-vector *pcl-files-map*)
              (for i from 1)
              (print i)
              (let ((wikidoc (wiki-parser:parse :dokuwiki
                                                (pcl-source-path (third chapter)))))
                (tt:draw-pages 
                 (tt:compile-text ()
                   (tt:with-style (:font rulisp::*base-font* :font-size rulisp::*font-size*)
                     (rulisp::pdf-render-wiki-item wikidoc)))
                   :break :after
                   :margins '(30 50 30 40)
                   :finalize-fn #'(lambda (page)
                                    (pdf:draw-centered-text (/ (aref (pdf::bounds page) 2) 2)
                                                            10
                                                            (write-to-string (incf page-number))
                                                            rulisp::*base-font*
                                                            10))))
                (pdf:write-document out))))))

(restas:define-route pcl-pdf ("pcl.pdf")
  (merge-pathnames "pcl.pdf"
                   *pcl-snapshot-dir*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load snapshot from http://pcl.catap.ru/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun load-pcl-snapshot ()
;;   (let ((snapshot-path (ensure-directories-exist (merge-pathnames (car (last (puri:uri-parsed-path *pcl-snapshot-url*)))
;;                                                                   *pcl-snapshot-dir*)))
;;         (snapshot (drakma:http-request *pcl-snapshot-url*
;;                                        :force-binary t)))
;;     (when snapshot
;;       (with-open-file (out
;;                        snapshot-path
;;                        :direction :output
;;                        :element-type '(unsigned-byte 8)
;;                        :if-exists :supersede)
;;         (write-sequence snapshot out))
;;       (zip:unzip snapshot-path
;;                  *pcl-snapshot-dir*
;;                  :if-exists :supersede)
;;       (setf *pcl-dir*
;;             (merge-pathnames "var/www/pcl.catap.ru/htdocs/data/pages/pcl/"
;;                              *pcl-snapshot-dir*))
        
;;       (make-pcl-pdf (merge-pathnames "pcl.pdf.tmp"
;;                                      *pcl-snapshot-dir*))
;;       (sb-posix:rename (merge-pathnames "pcl.pdf.tmp"
;;                                         *pcl-snapshot-dir*)
;;                        (merge-pathnames "pcl.pdf"
;;                                         *pcl-snapshot-dir*))
;;       t)))

;; (if *pcl-load-snapshot-p*
;;     (clon:schedule-function 'load-pcl-snapshot
;;                             (clon:make-scheduler (clon:make-typed-cron-schedule :hour '*)
;;                                                  :allow-now-p t)
;;                             :thread t))
