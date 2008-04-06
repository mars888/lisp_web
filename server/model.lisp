(in-package :server-model)

(defvar *model-specs* (make-hash-table :test 'equal))


(defclass DBModel ()
  ((name        :initarg :name        :initform nil  :accessor name)
   (plural-name :initarg :plural-name :initform nil  :accessor plural-name)
   (table-name  :initarg :table-name  :initform nil  :accessor table-name)
   (fields      :initarg :fields      :initform nil  :accessor fields)))
(defmethod print-object ((db-model DBModel) stream)
  (print-unreadable-object (db-model stream :identity t :type t)
    (format stream "name: ~A table-name: ~A fields: ~A" (name db-model) (table-name db-model) (fields db-model))))

(defmethod plural-name ((db-model DBModel))
  (or (slot-value db-model 'plural-name)
      (name db-model)))



(defclass DBField ()
  ((field-name :initarg :field-name  :reader field-name)
   (field-type :initarg :field-type  :reader field-type  :type string))
  (:documentation "Represents a database field for scaffolding."))
(defmethod print-object ((db-field DBField) stream)
  (print-unreadable-object (db-field stream :identity t :type t)
    (format stream "field-name: ~A field-type: ~A" (field-name db-field) (field-type db-field))))

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


(defclass BaseModel ()
  ()
  (:documentation "Baseclass of all models."))

(defgeneric keyfieldp (db-field)
  (:documentation "Return true if this passed field is a primary key field, nil otherwise"))
(defmethod keyfieldp ((db-field DBField))
  nil)
(defmethod keyfieldp ((db-field KeyField))
  t)


(defun get-field-instance-for-spec (spec)
  "Turn a clsql field specification into a derivative class instance of DBField.
   The type of the clsql field will determine the type of the DBField instance."
  (let ((kind (getf (cdr spec) :db-kind))
	(type (getf (cdr spec) :type 'string)))
    ;; If compounded type (such as (varchar 255)), we only need the first.
    (typecase type
      (cons (setf type (first type))))
    (case kind
      (:key (make-instance 'keyfield :field-name (first spec) :field-type "integer"))
      (:join (make-instance 'joinfield :field-name (first spec) :field-type "join"))
      (t (let ((type-string (string-downcase (symbol-name type))))
	   (cond 
	     ((equal type-string "varchar") (make-instance 'stringfield :field-name (first spec) :field-type type-string))
	     ((equal type-string "string")  (make-instance 'textareafield :field-name (first spec) :field-type type-string))
	     ((equal type-string "integer") (make-instance 'stringfield :field-name (first spec) :field-type type-string))
	     ((equal type-string "float")   (make-instance 'stringfield :field-name (first spec) :field-type type-string))
	     ((equal type-string "join")    (make-instance 'joinfield   :field-name (first spec) :field-type type-string))
	     (t (error (format nil "Unknown type for field ~A => ~A (~A)" spec (symbol-name type) (intern (symbol-name type)))))))))))

(defun add-model-spec (name spec &optional options-spec)
  (let ((db-model (make-instance 'dbmodel :name name)))
    (loop for item in spec do
	 (push (get-field-instance-for-spec item) (fields db-model)))
    (setf (table-name db-model)
	  (getf options-spec :base-table))
    (setf (plural-name db-model)
	  (getf options-spec :plural-name))
    (setf (fields db-model)
	  (nreverse (fields db-model)))
    (setf (gethash name *model-specs*) db-model)))

(defun find-model-spec (name)
  (gethash name *model-specs*))

(defun list-model-specs ()
  "List all specs that are registered."
  (loop for k being the hash-keys in *model-specs* do
       (format t "~A => ~%~A~%" k (gethash k *model-specs*))))

(defun find-key-field (name)
  "Try to find the primary key field for the class with the given name."
  (let ((spec (find-model-spec name)))
    (unless spec (error (format nil "Can't find spec for ~A" name)))
    (find-if (lambda (field) (keyfieldp field))
	     (fields spec))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special finders.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deffinder-id (class-name)
  "Create a finder function for finding elements (by id) for the class with the given name.
   For example:
     (deffinder-id book)
   will create a find named find-book for the model book taking an id parameter. E.g.:
     (find-book-by-id 10) => #<BOOK title:\"Java Sucks\" {11EFB341}>"
  (let ((spec (find-model-spec class-name))
	(id-field (find-key-field class-name)))
    `(defun ,(intern (format nil "~@:(find-~A-by-id~)" (name spec))) (id)
       (car (clsql:select ',class-name :where (clsql:sql-= (clsql:sql-slot-value ',class-name ',(field-name id-field)) 
							   (clsql:sql id))
			               :flatp t)))))

(defmacro deffinder-all (class-name)
  "Define a finder function for the model class with the given name that will fetch items of that class.
   Extra arguments can be supplied to further extend the query. E.g.:
   (find-all-book :where (clsql:sql-< (clsql:sql-slot-value 'book 'times-read) 10))"
  (let ((spec (find-model-spec class-name)))
    `(defun ,(intern (format nil "~@:(find-all-~A~)" (plural-name spec))) (&rest arguments)
       (push ',class-name arguments)
       (apply #'clsql:select arguments))))

(defmacro deffinders (class-name)
  "Define default findiers for model class given by class-name"
  `(progn
     (deffinder-id ,class-name)
     (deffinder-all ,class-name)))

(defmacro defmodel (class direct-superclasses slots &rest options)
  (push 'basemodel direct-superclasses)
  `(progn
     (eval-when  (:compile-toplevel :load-toplevel :execute)
       (add-model-spec ',class (quote ,slots) (quote ,@options)))
     (clsql:def-view-class ,class ,direct-superclasses ,slots ,@options)
     (deffinders ,class)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formatting and to-xml.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro format-with-indent (indent stream &rest args)
  "Just like format, but outputs `indent' amount of spaces before the line."
  (let ((times-var-sym (gensym "times-var"))
	(indent-sym    (gensym "indent"))
	(stream-sym    (gensym "stream")))
    `(progn
       (let ((,indent-sym ,indent)
	     (,stream-sym ,stream))
	 (dotimes (,times-var-sym ,indent-sym)
	   (format ,stream-sym " "))
	 (format ,stream-sym ,@args)))))

(defmacro with-tag ((name stream &key (indenting 0)) &body body)
  "Creates a block and surrounds that block with the tags as defined by `name'."
  (let ((name-sym      (gensym "name"))
	(stream-sym    (gensym "stream"))
	(indenting-sym (gensym "indenting")))
    `(progn (let ((,name-sym      ,name)
		  (,stream-sym    ,stream)
		  (,indenting-sym ,indenting))
	      (format-with-indent ,indenting-sym ,stream-sym "<~A>" ,name-sym)
	      ,@body
	      (format ,stream-sym "</~A>~%" ,name-sym)))))





(defgeneric to-xml (object &key stream related indenting include))

(defmethod to-xml ((object t) &key (stream *standard-output*) related (indenting 0) (include '()))
  (declare (ignore related)
	   (ignore include)
	   (ignore indenting))
  (if object
      (format stream "~A" object)))

(defmethod to-xml ((object cons) &key (stream *standard-output*) related (indenting 0) (include '()))
  (declare (ignore related))
  (with-tag ("list" stream :indenting indenting)
    (dolist (item object)
      (to-xml item :stream stream :indenting (+ 2 indenting) :include include))))

(defmethod to-xml ((object BaseModel) &key (stream *standard-output*) related (indenting 0) (include '()))
  (declare (ignore related))
  (let* ((model-spec (class-name (class-of object)))
	 (spec (gethash model-spec *model-specs*)))
    (with-tag ((string-downcase (symbol-name model-spec)) stream :indenting indenting)
      (loop for field-spec in (fields spec) do
	   (to-xml field-spec :stream stream :related object :indenting (+ 2 indenting) :include include)))))

(defmethod to-xml ((object DBField) &key (stream *standard-output*) related (indenting 0) (include '()))
  (with-tag ((string-downcase (field-name object)) stream :indenting indenting)
    (if related 
	(to-xml (slot-value related (field-name object)) :stream stream :include include))))

(defmethod to-xml ((object JoinField) &key (stream *standard-output*) related (indenting 0) (include '()))
  (let ((field-value (slot-value related (field-name object))))
    (if (find (field-name object) include)
	(typecase field-value
	  (cons (dolist (item field-value)
		  (to-xml item :stream stream :related related :indenting indenting)))
	  (t      (to-xml field-value :stream stream :related related :indenting indenting))))))


