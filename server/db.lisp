(in-package :server-db)

(clsql:locally-enable-sql-reader-syntax)

(eval-when (:compile-toplevel :load-toplevel)
  (format t "Adding MYSQL path to CLSQL library path")
  (clsql:push-library-path #P"/opt/local/lib/mysql5/mysql/")
  (asdf:operate 'asdf:load-op 'clsql-mysql))

(defun open-database (host name user password)
  (if (not (server-config:config-loaded-p))
      (server-config:load-default-config))
  (let ((db (clsql:connect (list host name user password) :database-type :mysql :pool t)))
    (clsql:start-sql-recording)
    db))

(defun close-database (db)
  (clsql:disconnect :database db))

(defun open-config-database ()
  (let ((host (server-config:get-item "db-host"))
	(name (server-config:get-item "db-name"))
	(user (server-config:get-item "db-user"))
	(pass (server-config:get-item "db-pass")))
    ;; (format t "~A ~A ~A ~A~%" host name user pass)
    (open-database host name user pass)))

(defmacro with-config-database ((database-var-name) &body body)
  `(let ((,database-var-name (open-config-database)))
     (unwind-protect 
	  (progn ,@body)
       (close-database ,database-var-name))))

(clsql:def-view-class Country ()
  ((id :db-kind :key
       :db-constraints :not-null
       :type integer
       :initarg :id
       :reader id)
   (name :type (clsql:varchar 255)
	 :initarg :name
	 :accessor name))
  (:base-table "countries"))

(defmethod print-object ((self Country) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "[~A]: \"~A\"" (id self) (name self))))

;; (clsql:def-view-class Architect ()
;;   ((id :db-kind :key
;;        :db-constraints :not-null
;;        :type integer
;;        :initarg :id
;;        :reader id)
;;    (office-name :type (clsql:varchar 255)
;; 		:initarg :office-name
;; 		:accessor office-name)
;;    (country-id  :type integer
;; 		:initarg :country-id
;; 		:initform nil
;; 		:accessor country-id
;; 		:column "country_id")
;;    (country     :db-kind :join
;; 		:db-info (:join-class country
;; 			  :home-key country-id
;; 			  :foreign-key id
;; 			  :set nil)
;; 		:accessor country))
;;   (:base-table "architects"))

;; (with-default-database (db)
;;   (clsql:start-sql-recording :type :both :database db)
;;   (let ((as (clsql:select 'architect :flatp t :caching nil)))
;;     (loop for a in as do
;; 	 (format t "~A ~A ~A => ~A~%" (id a) (office-name a) (country-id a) (country a)))))

