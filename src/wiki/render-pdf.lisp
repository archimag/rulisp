;;;; render-pdf.lisp

(in-package :rulisp)

(defparameter *wiki-pdf-render-map* (make-hash-table))

(defparameter *base-font* (pdf:get-font (pdf:font-name (pdf:load-ttf-file #P"/usr/share/fonts/corefonts/verdana.ttf"))))
(defparameter *bold-font* (pdf:get-font (pdf:font-name (pdf:load-ttf-file #P"/usr/share/fonts/corefonts/verdanab.ttf"))))
(defparameter *italic-font* (pdf:get-font (pdf:font-name (pdf:load-ttf-file #P"/usr/share/fonts/corefonts/verdanai.ttf"))))
(defparameter *monospace-font* (pdf:get-font (pdf:font-name (pdf:load-ttf-file #P"/usr/share/fonts/corefonts/cour.ttf"))))

(defparameter *font-size* 12)


(defun pdf-render-wiki-item (item)
  (cond
    ((and (consp item)
          (symbolp (car item))) (let ((render (gethash (car item) *wiki-pdf-render-map*)))
                                  (if render
                                      (funcall render (cdr item))
                                      (pdf-render-wiki-item (cdr item)))))
    ((consp item) (iter (for i in item)
                        (pdf-render-wiki-item i)))
    ((symbolp item) (let ((render (gethash item *wiki-pdf-render-map*)))
                      (if render
                          (funcall render nil))))
    ((stringp item) (tt:put-string item))))

(defun pdf-render-all-wiki-items (items)
  (iter (for item in items)
        (pdf-render-wiki-item item)))


(defun pdf-render-wiki-page (wikidoc out)
  (tt:with-document (:mode :outlines)
    (tt:draw-pages
     (tt:compile-text ()
       (tt:with-style (:font *base-font* :font-size *font-size*)       
         (pdf-render-wiki-item wikidoc)))
     :break :after
     :margins '(30 50 30 40))
    (pdf:write-document out)))


(defmacro define-wiki-pdf-render (name (items) &body body)
  `(setf (gethash ',name
                  *wiki-pdf-render-map*)
         (lambda (,items)
           ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *header-font-size* 18)

(defvar *current-chapter* nil)

(defclass chapter-header ()
  ((title :initarg :title)
   (parent :initarg :parent :initform nil)))

(defun find-outline-by-ref (ref outline)
  (if (eql (pdf::reference outline) ref)
      outline
      (iter (for item in (pdf::sub-levels outline))
            (let ((res (find-outline-by-ref ref item)))
              (finding res such-that res)))))

(defmethod tt::stroke ((header chapter-header) x y)
  (let ((parent-ref (slot-value header 'parent)))
    (pdf:append-child-outline (or (and parent-ref
                                       (find-outline-by-ref (pdf::get-named-reference parent-ref)
                                                            (pdf:outline-root pdf:*document*)))
                                  (pdf:outline-root pdf:*document*))
                              (slot-value header 'title)
                              (pdf::register-named-reference (vector pdf:*page* "/FitH" y)
                                                             (slot-value header 'title)))))

(define-wiki-pdf-render dokuwiki:chapter (items)
  (let ((name (second (first items))))
    (tt::add-box (make-instance 'chapter-header
                                :title name
                                :parent *current-chapter*))
    (tt:with-style (:font *bold-font* :font-size *header-font-size*)
      (tt:paragraph (:top-margin 10)
        (tt:put-string name)))
    (let ((*header-font-size* (- *header-font-size* 2))
          (*current-chapter* name))
      (pdf-render-all-wiki-items (cdr items)))))

(defvar *paragraph* nil)

(define-wiki-pdf-render dokuwiki:eol (items)
  (tt:put-string "
")
  (pdf-render-all-wiki-items items))

(define-wiki-pdf-render dokuwiki:paragraph (items)
  (tt:paragraph (:bottom-margin 5 :top-margin 5)
    (let ((*paragraph* t))
      (pdf-render-all-wiki-items items))))

(define-wiki-pdf-render dokuwiki:footnote (items)
  (declare (ignore items)))

(define-wiki-pdf-render dokuwiki:linebreak (items)
  (declare (ignore items))
  (tt:new-line))

(define-wiki-pdf-render dokuwiki:monospace (items)
  (tt:with-style (:font *monospace-font*)
    (pdf-render-all-wiki-items items)))

(define-wiki-pdf-render dokuwiki:strong (items)
  (tt:with-style (:font *bold-font*)
    (pdf-render-all-wiki-items items)))

(define-wiki-pdf-render dokuwiki:emphasis (items)
  (tt:with-style (:font *italic-font*)
    (pdf-render-all-wiki-items items)))

(defun decoration-underline (box x y dx dy)
  (declare (ignore box))
  (pdf:with-saved-state
    (pdf:set-color-stroke tt::*color*)
    (pdf:set-line-width (* 0.06 *font-size*))
    (pdf:move-to x (+ y (* 0.9 dy)))
    (pdf:line-to (+ x dx) (+ y (* 0.9 dy)))
    (pdf:stroke)))

(define-wiki-pdf-render dokuwiki:underline (items)
  (tt:with-style (:post-decoration #'decoration-underline)
    (pdf-render-all-wiki-items items)))

(defun show-code (code)
  (let ((font-size (- *font-size* 2)))
  (typeset:table (:col-widths '(500) :splittable-p nil :border 0.1 :border-color #xD0D0D0)
    (typeset:row (:background-color #xEEEEEE )
      (typeset:cell ()
        (tt:paragraph (:font-size font-size)
          (tt:vspace 4)
          (let* ((lines (split-sequence:split-sequence #\Newline
                                                       code
                                                       :remove-empty-subseqs t))
                 (min-space-count (iter (for line in lines)
                                        (minimizing (or (position #\Space line :test-not #'char-equal)
                                                        0)))))
            (let ((tt::*font* *monospace-font*))
              (iter (for line in lines)
                    (when (and line
                               (not (string= line "")))
                      (tt:hspace (+ 4
                                    (* (pdf:get-char-size #\Space *monospace-font* font-size)
                                       (- (or (position #\Space line :test-not #'char-equal)
                                              0)
                                          min-space-count))))
                      (tt:put-string line))
                    (tt:new-line))))
          (tt:vspace 4)))))))

(define-wiki-pdf-render dokuwiki:code (items)
  (show-code (first items)))

(define-wiki-pdf-render dokuwiki:preformatted (items)
  (show-code (first items)))

(define-wiki-pdf-render dokuwiki:quoted (items)
  (tt:paragraph (:left-margin 20)
    (tt:with-style (:font *italic-font*)
      (pdf-render-all-wiki-items items))))


(define-wiki-pdf-render dokuwiki:unordered-listblock (items)
  (iter (for item in items)
        (tt:paragraph (:left-margin 10)
          (tt:put-string "*")
          (pdf-render-wiki-item item))))

(define-wiki-pdf-render dokuwiki:ordered-listblock (items)
  (iter (for item in items)
        (for num from 1)
        (tt:paragraph (:left-margin 10)
          (tt:put-string (format nil "~A." num))
          (pdf-render-wiki-item item))))

(define-wiki-pdf-render dokuwiki:en-dash (items)
  (declare (ignore items))
  (tt:put-string (string +EN-DASH+)))

(define-wiki-pdf-render dokuwiki:em-dash (items)
  (declare (ignore items))
  (tt:put-string (string +EM-DASH+)))

(define-wiki-pdf-render dokuwiki:external-link (items)
  (tt:with-style (:post-decoration #'decoration-underline :color :blue)
    (pdf-render-all-wiki-items items)))

(define-wiki-pdf-render dokuwiki:internal-link (items)
  (let ((delimiter (position #\| (car items))))
    (tt:with-style (:post-decoration #'decoration-underline :color :blue)
      (tt:put-string (string-trim '#(#\Space #\Tab)
                                  (if delimiter
                                      (subseq (car items) (1+ delimiter))
                                      (car items)))))
    (when delimiter
      (tt:with-style (:font *italic-font*)
        (tt:put-string (format nil
                               " <~A>"
                               (string-trim '#(#\Space #\Tab)
                                            (subseq (car items) 0 delimiter))))))))


(define-wiki-pdf-render dokuwiki:table (items)
  (typeset:table (:col-widths '(500) :splittable-p nil :border 0.1 :border-color #xD0D0D0)
    (typeset:row (:background-color #xEEEEEE )
      (typeset:cell ()
        (tt:paragraph ()
          (let ((tt::*font* *bold-font*))
            (tt:put-string "Здесь должна быть таблица, но я пока не придумал способа её отображения с корректным layout, извините."))
          (tt:new-line)
          (tt:put-string "archimag"))))))
          


    