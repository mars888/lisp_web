(in-package :server-scaffold)

#|
 | Label generators .
 |#
(defgeneric generate-label (dbfield stream))

(defmethod generate-label ((dbfield DBField) stream)
  (cl-who:with-html-output (stream)
    (:label (cl-who:str (string-capitalize (string-downcase (field-name dbfield)))))))

(defmethod generate-label ((dbfield KeyField) stream)
  "Do nothing")

(defmethod generate-label ((dbfield RelationField) stream)
  "Do nothing")

#|
 | Field generators.
 |#
(defgeneric generate-field (dbfield stream instance))

(defmethod generate-field ((dbfield DBField) stream instance)
  "Do nothing")

(defun field-name-for (dbfield)
  (string-downcase (field-name dbfield)))

(defmethod generate-field ((dbfield EditableField) stream instance)
  (cl-who:with-html-output (stream)
    (:input :name (field-name-for dbfield)
	    :value (if instance
		       (cl-who:esc (slot-value instance (field-name dbfield)))
		       "")
	    :class "text")))

(defmethod generate-field ((dbfield TextAreaField) stream instance)
  (cl-who:with-html-output (stream)
    (:textarea :name (field-name-for dbfield)
	       (if instance
		   (cl-who:esc (slot-value instance (field-name dbfield)))
		   ""))))

(defun scaffold-form (stream model-name &optional (instance nil))
  "Generate form fields for the given model-spec."
  (let ((spec (find-model-spec model-name)))
    (cl-who:with-html-output (stream)
      (loop for field-spec in spec do
	   (generate-label field-spec stream)
	   (generate-field field-spec stream instance)
	   (format stream "~%")))))



#|
 | Scaffolding
 |#
(defmodel measurement ()
  ((id :type integer
       :initarg :id
       :initform nil
       :db-kind :key
       :reader id)
   (name :type string
	 :initarg :name
	 :initform ""
	 :accessor name))
  (:base-table "measurements"
   :plural-name "measurements"))


(defmacro defscaffold-index (model-name)
  (let ((spec (find-model-spec model-name)))
    `(server-url:defaction ,(intern (format nil "~@:(~A-index~)" (plural-name spec))) () (,(format nil "~(~A~)" (plural-name spec)))
       (with-config-database (db)
	 (let ((items (clsql:select ',model-name :flatp t)))
	   (cl-who:with-html-output-to-string (s nil :indent t)
	     (:table
	      (loop for item in items do
		   (cl-who:htm (:tr
				,@(loop for field in (fields spec)
				     collecting `(:td (cl-who:str (slot-value item ',(field-name field)))))
				(:td "Link goes here")))))))))))

(defscaffold-index measurement)