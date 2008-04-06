(defpackage :test
  (:use :cl
        :clsql
        :cl-who
        :server-model
	:server-control
	:server-db))

(in-package :test)

(clsql:enable-sql-reader-syntax)

;; MODEL: Measurement.
(defmodel measurement ()
  ((id      :type integer      :initarg :id    :initform nil  :db-kind :key  :reader id)
   (name    :type string       :initarg :name  :initform ""   :accessor name)
   (samples :accessor samples  :db-kind :join  :db-info (:join-class sample  :home-key id  :foreign-key measurement-id)))
  (:base-table "measurements"))

;; MODEL: Sample.
(defmodel sample ()
  ((id             :type integer  :initarg :id              :initform nil        :db-kind :key  :reader id)
   (x              :type float    :initarg :x               :initform 0.0        :accessor x)
   (y              :type float    :initarg :y               :initform 0.0        :accessor y)
   (z              :type float    :initarg :z               :initform 0.0        :accessor z)
   (raw            :type string   :initarg :raw             :initform "Unknown"  :accessor raw)
   (density-c      :type float    :initarg :density-c       :initform 0.0        :accessor density-c  :column "density_c")
   (density-m      :type float    :initarg :density-m       :initform 0.0        :accessor density-m  :column "density_m")
   (density-y      :type float    :initarg :density-y       :initform 0.0        :accessor density-y  :column "density_y")
   (density-k      :type float    :initarg :density-k       :initform 0.0        :accessor density-k  :column "density_k")
   (measurement-id :type integer  :initarg :measurement-id  :initform nil        :accessor measurement-id  :column "measurement_id")
   (measurement    :accessor measurement  :db-kind :join  :db-info (:join-class measurement  :home-key measurement-id  :foreign-key id  :set nil)))
  (:base-table "samples"))

(defun test-sample-select ()
  (princ (with-config-database (db)
					;(to-xml (car (select 'sample :where [= [slot-value 'sample 'id] 805] :flatp t :caching nil)))
	   (to-xml (car (select 'sample :where (sql-= (sql-expression :table "samples" :attribute "id") 805) :flatp t))))))

(defun test-measuement-select ()
  (with-config-database (db)
      (to-xml (car (select 'measurement :where (sql-= (sql-expression :table "measurements" :attribute "id") 56) :flatp t)) :include '(samples))))


(server-url:defaction list-measurements () ("measurements")
  (setf (hunchentoot:content-type) "text/xml")
  (with-config-database (db)
    (let ((measurements (select 'measurement :flatp t)))
      (with-output-to-string (s)
	(format s "<?xml version=\"1.0\" ?>~%")
	(to-xml measurements :stream s :include '(samples))))))

(clsql:disable-sql-reader-syntax)








