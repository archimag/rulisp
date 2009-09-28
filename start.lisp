;;; start.lisp

(defpackage :rulisp.starter
  (:use :cl)
  (:export #:rulisp-start #:rulisp-stop))

(in-package :rulisp.starter)

(defun rulisp-dispatcher (req)
  (if (string= (hunchentoot:host req) rulisp.preferences:*host*)
      (restas::restas-dispatcher req)
      (hunchentoot:redirect (hunchentoot:request-uri req)
                            :host rulisp.preferences:*host*)))

(defclass rulisp-acceptor (restas::restas-acceptor) ())

(defmethod initialize-instance :after ((acceptor rulisp-acceptor) &key)
  (setf (hunchentoot:acceptor-request-dispatcher acceptor)
        'rulisp-dispatcher))


(defparameter *acceptor* nil)

(defun rulisp-start ()
  (when *acceptor*
    (error "web server has already been started"))
  (restas::reconnect-all-sites)
  (setf *acceptor*
        (hunchentoot:start (make-instance 'rulisp-acceptor
                                          :port (let ((port (second (split-sequence:split-sequence #\: rulisp.preferences:*host* ))))
                                                  (if port
                                                      (parse-integer port)
                                                      80)))))
  (print "rulisp started"))

(defun rulisp-stop ()
  (unless *acceptor*
    (error "web server not yet been started"))
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))





  