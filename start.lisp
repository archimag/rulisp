;;; start.lisp

(defpackage :rulisp.starter
  (:use :cl)
  (:export :run))

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

;;(restas:reconnect-all-plugins)
(restas::reconnect-all-sites)

(if *acceptor*
    (error "web server has already been started")
    (setf *acceptor*
          (hunchentoot:start (make-instance 'rulisp-acceptor
                                            :port (let ((port (second (split-sequence:split-sequence #\: rulisp.preferences:*host* ))))
                                                    (if port
                                                        (parse-integer port)
                                                        80))))))
