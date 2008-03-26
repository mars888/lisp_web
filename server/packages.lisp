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

(defpackage server-config
  (:use :cl)
  (:export :config-loaded-p
           :load-config
	   :load-default-config
	   :print-config
	   :has-item
	   :get-item))

(defpackage server-db
  (:use :cl)
  (:export :open-database
	   :open-default-database
	   :close-database
	   :with-default-database))

(defpackage server-model
  (:use :cl)
  (:export :defmodel
	   :scaffold-form))