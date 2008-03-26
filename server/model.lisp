(in-package :server-model)

(defvar *model-specs* (make-hash-table :test 'equal))

(defclass DBField ()
  ((field-name :initarg :field-name
	       :reader field-name))
  (:documentation "Represents a database field for scaffolding."))

(defclass KeyField (DBField)
  ()
  (:documentation "Primary key field"))


(defclass EditableField (DBField)
  ()
  (:documentation "All fields that are editable should inherit from this."))

(defclass StringField (EditableField)
  ())

(defclass TextAreaField (EditableField)
  ())


(defclass RelationField (DBField)
  ()
  (:documentation "All fields that represent relations should inherit from this."))

(defclass JoinField (RelationField)
  ())

(defun get-field-instance-for-spec (spec)
  "Turn a clsql field specification into a derivative class instance of DBField.
   The type of the clsql field will determine the type of the DBField instance."
  (let ((kind (getf (cdr spec) :db-kind))
	(type (getf (cdr spec) :type 'string)))
    ;; If compounded type (such as (varchar 255)), we only need the first.
    (typecase type
      (cons (setf type (first type))))
    (case kind
      (:key (make-instance 'keyfield :field-name (first spec)))
      (:join (make-instance 'joinfield :field-name (first spec)))
      (t (let ((type-string (string-downcase (symbol-name type))))
	   (cond 
	     ((equal type-string "varchar") (make-instance 'stringfield :field-name (first spec)))
	     ((equal type-string "string")  (make-instance 'textareafield :field-name (first spec)))
	     (t (error (format nil "Unknown type for field ~A => ~A (~A)" spec (symbol-name type) (intern (symbol-name type)))))))))))

(defun add-model-spec (name spec)
  (let ((fields '()))
    (loop for item in spec do
	 (push (get-field-instance-for-spec item) fields))
    (setf (gethash name *model-specs*) (nreverse fields))))



(defmacro defmodel (class direct-superclasses slots &rest options)
  `(progn 
     (add-model-spec ',class (quote ,slots))
     (clsql:def-view-class ,class ,direct-superclasses ,slots ,@options)))

;; (defmodel UserGroupSection ()
;;   ((id :db-kind :key
;;        :db-constraints :not-null
;;        :type integer
;;        :initarg :id
;;        :reader id)
;;    (name :type (clsql:varchar 255)
;; 	 :initarg :name
;; 	 :accessor name)
;;    (symbol :type (clsql:varchar 255)
;; 	   :initarg :symbol
;; 	   :accessor symbol)
;;    (description :type (string 32000)
;; 		:initarg :description
;; 		:accessor description)))


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

(defun scaffold-form (stream model-spec &optional (instance nil))
  "Generate form fields for the given model-spec."
  (let ((spec (gethash model-spec *model-specs*)))
    (cl-who:with-html-output (stream)
      (loop for field-spec in spec do
	   (generate-label field-spec stream)
	   (generate-field field-spec stream instance)))))






