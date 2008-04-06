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
	   :with-config-database))

(defpackage server-model
  (:use :cl)
  (:export :dbmodel
	   :name
	   :plural-name
	   :table-name
	   :fields

	   :dbfield
	   :field-name
	   :field-type

	   :keyfield
	   :editablefield
	   :stringfield
	   :textareafield
	   :relationfield
	   :joinfield
	   :basemodel
	   :keyfieldp

	   :find-model-spec
	   :list-model-specs
	   :find-key-field
           :defmodel
	   :to-xml))

(defpackage server-scaffold
  (:use :cl
	:server-db
	:server-model)
  (:export :defscaffold
	   :defscaffold-index
	   :defscaffold-show))