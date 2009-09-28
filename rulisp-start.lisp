;;; rulisp-start.lisp


;; Report PID information.
(with-open-file (out "/var/rulisp/rulisp.pid"
                 :direction :output
                 :if-exists :supersede)
  (write-string (sb-unix::posix-getenv "STY") out))

;; Load ASDF 
(require :asdf)

(push #P"/usr/share/rulisp/systems/" asdf:*central-registry*)

(let ((*compile-print* nil)
      (*compile-verbose* nil))
  (handler-bind ((warning #'muffle-warning))
    (asdf:oos 'asdf:load-op :asdf-binary-locations)))

(setf asdf:*centralize-lisp-binaries* t)

(setf asdf:*default-toplevel-directory* #P"/var/rulisp/.fasl")

;; load swank
(asdf:oos 'asdf:load-op :swank)

;; Communication will be established through a single stream.
(setf swank:*use-dedicated-output-stream* nil)

;; Fire up a fresh swank server.
(swank:create-server :port 10010
                     :coding-system "utf-8-unix"
                     :dont-close t)

;; Start rulisp server
(asdf:operate 'asdf:load-op :rulisp)
(rulisp.starter:rulisp-start)