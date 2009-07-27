
(in-package :rulisp)

(restas:start-web-server (let ((port (second (split-sequence:split-sequence #\: *host* ))))
                           (if port
                               (parse-integer port)
                               80)))
(restas:reconnect-all-plugins)