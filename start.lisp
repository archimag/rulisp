
(in-package :cl-user)

(restas:start-web-server (let ((port (second (split-sequence:split-sequence #\: rulisp.preferences:*host* ))))
                           (if port
                               (parse-integer port)
                               80)))

(restas:reconnect-all-plugins)