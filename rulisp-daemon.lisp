;;;; rulisp-daemon.lisp
;;;; author: Moskvitin Andrey (archimag)
;;;;
;;;; "Pure lisp" SBCL daemon, which is used on the site http://lisper.ru/
;;;;
;;;; Usage:
;;;; sbcl --noinform --no-userinit --no-sysinit --load /path/to/rulisp-daemon.lisp COMMAND
;;;; where COMMAND one of: start stop zap kill restart
;;;;
;;;; If successful, the exit code is 0, otherwise 1
;;;;
;;;; Error messages look in /var/log/messages (usually, depend on syslog configuration)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :sbcl.daemon
  (:use :cl :sb-alien :sb-ext))

(in-package :sbcl.daemon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WARNING!
;;;; plantform-depends constant :(
;;;; changes for you platform... or make path for sbcl ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (boundp 'sb-unix:tiocnotty)
  (defconstant sb-unix:tiocnotty 21538))

(defconstant +PR_SET_KEEPCAPS+ 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; basic parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fasldir* #P"/var/cache/rulisp/fasl/")
(defparameter *pidfile* #P"/var/run/rulisp/rulisp.pid")
(defparameter *swank-port* 9999)
(defparameter *daemon-user* "rulisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-exit-on-error (&body body)
  `(handler-case (progn ,@body)
     (error (err)
       (with-output-to-string (*standard-output*)
       (let ((*print-escape* nil))
         (print-object err *error-output*)
         (write #\Newline :stream *error-output*)
         (sb-ext:quit :unix-status 1))))))

(defmacro with-silence (&body body)
  `(with-output-to-string (*trace-output*)
     (with-output-to-string (*standard-output*)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Processing command line arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; command-line COMMAND

(defvar *daemon-command* (second *posix-argv*))

;;;; quit if COMMAND is unknown

(unless (find *daemon-command* '("start" "stop" "zap" "kill" "restart") :test #'string-equal)
  (with-exit-on-error
    (error "Bad command-line options")))

;;;; zap - remove pid file

(when (string-equal *daemon-command* "zap")
  (with-exit-on-error     
    (delete-file *pidfile*)
    (sb-ext:quit :unix-status 0)))

;;;; stop - send to daemon sigusr1 signal, wait and remove pid file

(with-silence
  (require 'sb-posix))

(defun read-pid ()
  (with-open-file (in *pidfile*)
    (read in)))

(defun stop-daemon ()
  (let ((pid (read-pid)))
    (sb-posix:kill pid sb-posix:sigusr1)
    (loop
       while (not (null (ignore-errors (sb-posix:kill pid 0))))
       do (sleep 0.1)))
  (delete-file *pidfile*))

(when (string-equal *daemon-command* "stop")
  (with-exit-on-error 
    (stop-daemon)
    (sb-ext:quit :unix-status 0)))

;;;; kill - send to daemon kill signal and remove pid file

(when (string-equal *daemon-command* "kill")
  (with-exit-on-error
    (sb-posix:kill (read-pid)
                   sb-posix:sigkill)
    (delete-file *pidfile*)
    (sb-ext:quit :unix-status 0)))

;;;; restart daemon

(when (string-equal *daemon-command* "restart")
  (with-exit-on-error
    (stop-daemon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Start daemon!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; required path for sbcl :(
(sb-posix::define-call "grantpt" int minusp (fd sb-posix:file-descriptor))
(sb-posix::define-call "unlockpt" int minusp (fd sb-posix:file-descriptor))
(sb-posix::define-call "ptsname" c-string null (fd sb-posix:file-descriptor))
(sb-posix::define-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t))

(defun switch-to-slave-pseudo-terminal (&optional (out #P"/dev/null") (err #P"/dev/null"))
  (flet ((c-bit-or (&rest args)
           (reduce #'(lambda (x y) (boole boole-ior x y))
                   args)))
    (let* ((fdm (sb-posix:open #P"/dev/ptmx" sb-posix:O-RDWR))
           (slavename (progn
                        (sb-posix:grantpt fdm)
                        (sb-posix:unlockpt fdm)
                        (sb-posix:ptsname fdm)))
           (fds (sb-posix:open slavename sb-posix:O-RDONLY))
           (out-fd (sb-posix:open out
                               (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-TRUNC)
                               (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH)))
           (err-fd (if (not (equal err out))
                       (sb-posix:open err
                                      (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-TRUNC)
                                      (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH))
                       (if out (sb-posix:dup out-fd)))))
      (sb-posix:dup2 fds 0)
      (sb-posix:dup2 out-fd 1)
      (sb-posix:dup2 err-fd 2))))

(defun change-user (name &optional group)
  (let ((gid)
        (uid))
    (when group
      (setf gid
            (sb-posix:group-gid (sb-posix:getgrnam group))))
    (let ((passwd (sb-posix:getpwnam name)))
      (unless group
        (setf gid
              (sb-posix:passwd-gid passwd))
        (setf uid
              (sb-posix:passwd-uid passwd))))
    (sb-posix:setresgid gid gid gid)
    (sb-posix:initgroups name gid)
    (sb-posix:setresuid uid uid uid)))

(defvar *status* nil)

(defun signal-handler (sig info context)
  (declare (ignore info context))
  (setf *status* sig))

(sb-sys:enable-interrupt sb-posix:sigusr1 #'signal-handler)
(sb-sys:enable-interrupt sb-posix:sigchld #'signal-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; change uid and gid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; required for start hunchentoot on port 80
(sb-posix::define-call "prctl" int minusp (option int) (arg int))
(sb-posix:prctl +PR_SET_KEEPCAPS+ 1)

(change-user *daemon-user*)

;;;; required for start hunchentoot on port 80
(load-shared-object (find-if #'probe-file
                             '("/lib/libcap.so.2" "/lib/libcap.so")))

(sb-posix::define-call "cap_from_text" (* char) null-alien (text c-string))
(sb-posix::define-call "cap_set_proc" int minusp (cap_p (* char)))
(sb-posix::define-call "cap_free" int minusp (cap_p (* char)))

(let ((cap_p (sb-posix:cap-from-text "CAP_NET_BIND_SERVICE=ep")))
  (sb-posix:cap-set-proc cap_p)
  (sb-posix:cap-free cap_p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fork!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (= (sb-posix:fork) 0)
  (loop
     while (null *status*)
     do (sleep 0.1))
  (quit :unix-status (if (= *status* sb-posix:sigusr1)
                         0
                         1)))


(defparameter *ppid* (sb-posix:getppid))

;;;; set global error handler 
(defun global-error-handler (condition x)
  (declare (ignore x))
  (let ((err (with-output-to-string (out)
                     (let ((*print-escape* nil))
                       (print-object condition out)))))
    (print err *error-output*)
    (sb-posix:syslog sb-posix:log-err
                     err))
  (quit :unix-status 1))

(setf *debugger-hook* #'global-error-handler)

(sb-sys:enable-interrupt sb-posix:sigusr1 :default)
(sb-sys:enable-interrupt sb-posix:sigchld :default)

;;;; change current directory
(sb-posix:chdir #P"/")

;;;; umask
(sb-posix:umask 0)

;;;; detach from tty
(let ((fd (sb-posix:open #P"/dev/tty" sb-posix:O-RDWR)))
  (sb-posix:ioctl fd sb-unix:tiocnotty)
  (sb-posix:close fd))

;;;; rebind standart input, output and error streams
(switch-to-slave-pseudo-terminal)

;;;; start new session
(sb-posix:setsid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load asdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :asdf)

(push #p"/usr/share/common-lisp/systems/" asdf:*central-registry*)
(push #P"/usr/share/rulisp/systems/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op :asdf-binary-locations)

(setf asdf:*centralize-lisp-binaries* t)

(setf asdf:*default-toplevel-directory* *fasldir*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start swank server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:oos 'asdf:load-op :swank)

(setf swank:*use-dedicated-output-stream* nil)

(swank:create-server :port *swank-port*
                     :coding-system "utf-8-unix"
                     :dont-close t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Start rulisp server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :rulisp)
(rulisp.starter:rulisp-start)

(sb-sys:enable-interrupt sb-posix:sigusr1
                         #'(lambda (sig info context)                             
                             (declare (ignore sig info context))
                             (handler-case
                                 (progn 
                                   (sb-posix:syslog sb-posix:log-info "Stop rulisp daemon")
                                   (rulisp.starter:rulisp-stop))
                               (error (err)
                                 (sb-posix:syslog sb-posix:log-err
                                                  (with-output-to-string (out)
                                                    (let ((*print-escape* nil))
                                                      (print-object err out))))))
                             (sb-ext:quit :unix-status 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end daemon initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; write pid file
(with-open-file (out *pidfile* :direction :output :if-exists :error :if-does-not-exist :create)
  (write (sb-posix:getpid) :stream out))

(sb-posix:kill *ppid* sb-posix:sigusr1)
(setf *debugger-hook* nil)

(sb-posix:syslog sb-posix:log-info "Start rulisp daemon")


