;;;; render-pdf.lisp

(in-package :rulisp)

(defparameter *wiki-pdf-render-map* (make-hash-table))

(defparameter *base-font* (pdf:get-font (pdf:font-name (pdf:load-ttf-file #P"/usr/share/fonts/corefonts/verdana.ttf"))))
(defparameter *bold-font* (pdf:get-font (pdf:font-name (pdf:load-ttf-file #P"/usr/share/fonts/corefonts/verdanab.ttf"))))
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

(define-wiki-pdf-render dokuwiki:chapter (items)
  (pdf:with-outline-level ((second (first items))
                           (pdf:register-page-reference))
    (tt:with-style (:font *bold-font* :font-size 16)
      (tt:paragraph (:top-margin 10)
        (tt:put-string (second (first items)))))
    (pdf-render-all-wiki-items (cdr items))))

(define-wiki-pdf-render dokuwiki:paragraph (items)
  (tt:paragraph (:bottom-margin 5 :top-margin 5)
    (pdf-render-all-wiki-items items)))


(defun show-code (code)
  (typeset:table (:col-widths '(500) :splittable-p nil :border 0.1 :border-color #xD0D0D0)
    (typeset:row (:background-color #xEEEEEE )
      (typeset:cell ()
        (tt:paragraph (:font-size (- *font-size* 2))
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
                                    (* (pdf:get-char-size #\Space *monospace-font*)
                                       (- (or (position #\Space line :test-not #'char-equal)
                                              0)
                                          min-space-count))))
                      (tt:put-string line))
                    (tt:new-line))))
          (tt:vspace 4))))))

(define-wiki-pdf-render dokuwiki:code (items)
  (show-code (first items)))

(define-wiki-pdf-render dokuwiki:preformatted (items)
  (show-code (first items)))

(define-wiki-pdf-render dokuwiki:monospace (items)
  (pdf-render-all-wiki-items items))