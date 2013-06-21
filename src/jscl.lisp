;;;; jscl.lisp
;;;;
;;;; This file is part of the rulisp application, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>


(in-package :rulisp.jscl)

(restas:define-route jscl-main ("")
  (rulisp::rulisp-finalize-page :title "JSCL"
                                :css '("style.css" "jscl.css")
                                :js '("/apps/jscl/jscl.js" "/js/jquery.js" "/js/jqconsole.min.js" "/js/jsclREPL.js")
                                :content "<div id='console'></div>"))

(defun load-jscl-snapshot ()
  (ensure-directories-exist *jscl-snapshot-dir*)
  (asdf:run-shell-command "cd ~a && (git clone ~a || (cd jscl && git fetch && git merge --ff-only origin/master)) && cd jscl && sbcl --load jscl --quit --eval \"(jscl:bootstrap)\""
                          *jscl-snapshot-dir*
                          *jscl-snapshot-url*))

(restas:define-route jscl-js ("jscl.js")
  (let ((path (merge-pathnames "jscl/jscl.js" *jscl-snapshot-dir*)))
    (unless (fad:file-exists-p path)
      (load-jscl-snapshot))
    (hunchentoot:handle-static-file path "text/javascript")))

(when *jscl-load-snapshot-p*
  (clon:schedule-function 'load-jscl-snapshot
                          (clon:make-scheduler (clon:make-typed-cron-schedule :hour '*)
                                               :allow-now-p t)
                          :thread t))
