(in-package server-control)

(defvar *debug-mode* t)
(defvar *server* nil)
(defvar *hunchentoot-dispatch-table* '(main-handler))

(defun start-server (&key (port 8080))
  (server-config:load-default-config)
  (setf *server* (hunchentoot:start-server :port port))
  (setf hunchentoot:*dispatch-table* *hunchentoot-dispatch-table*)
  (when *debug-mode*
    (setf hunchentoot:*show-lisp-errors-p* t)
    (setf hunchentoot:*show-lisp-backtraces-p* t)
    ;; Use debugger:
    ; (setf hunchentoot:*catch-errors-p* nil)
    ;; Output headers on stdout:
    ; (setf hunchentoot:*header-stream* *standard-output*)
    ))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop-server *server*))
  (setf *server* nil))

(defun restart-server ()
  (if *server*
      (stop-server))
  (start-server))



(defun render-page ()
  (format nil "~A" hunchentoot:*request*))

(defun main-handler (request)
  (multiple-value-bind (handler hash-table) (server-url:find-match-for (hunchentoot:request-uri request))
    (if handler
	(lambda ()
	  (funcall (server-url:handler handler) hash-table))))
;;   (let ((handler (server-url:find-match-for (hunchentoot:request-uri request))))
;;     (hunchentoot:log-message :info (with-output-to-string (s)
;; 				     (format s "~A" handler)))
;;     (if handler
;; 	(server-url:handler handler)
;; 	nil))
)


