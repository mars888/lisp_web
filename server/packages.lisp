(defpackage server-control
  (:use :cl)
  (:export :start-server
	   :stop-server
	   :restart-server))

(defpackage server-url
  (:use :cl)
  (:export :name
	   :handler
	   :path
           :make-url
	   :register-url
	   :unregister-url
	   :clear-registered-urls
	   :show-registered-urls
	   :get-registered-urls
	   :match
	   :find-match-for
	   :build-url
	   :build-url-for
	   :defaction))