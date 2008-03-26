
#|
(defun handle-stuff (vars)
  (declare (ignore vars))
  "foo-bar")
(server-url:register-url (server-url:make-url :name "handle-stuff" :handler 'handle-stuff :path '("home" "show")))

(defun books-index (vars)
  (declare (ignore vars))
  "books here")
(server-url:register-url (server-url:make-url :name "books-index" :handler 'books-index :path '("books")))

(defun book-show (vars)
  (let ((id (gethash "id" vars)))
    (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
	(:html :xmlns "http://www.w3.org/1999/xhtml"
	 (:head 
	  (:title (cl-who:str (format nil "Book with ID: ~A" id))))
	 (:body
	  (:strong (cl-who:str (format nil "Book with ID: ~A" id)))
	  (:br)
	  (:a :href (cl-who:str (server-url:build-url-for "books-index")) "Go home")
	  (:br)
	  "Book with ID: " (:strong (cl-who:str id)))))))
(server-url:register-url (server-url:make-url :name "book-show" :handler 'book-show :path '("book" :id)))
|#

(server-url:defaction action-test (vars) ("action-test" :id)
  (format nil "action-test called with: ~A" (gethash "id" vars)))


(defmacro with-page-layout ((stream-name &key (title "Title")) &body body)
	  `(cl-who:with-html-output-to-string (,stream-name nil :prologue t :indent t)
	     (:html (:head :xmlns "http://www.w3.org/1999/xhtml"
		     (:title ,title))
		    (:body ,@body))))


(clsql:def-view-class Sample ()
  ((id :db-kind :key
       :db-constraints :not-null
       :type integer
       :initarg :id
       :reader id)
   (measurement-id :type integer
		   :initarg :measurement-id
		   :initform nil
		   :accessor measurement-id
		   :column "measurement_id")
   (x :type float
      :initarg :x
      :initform nil
      :accessor x)
   (y :type float
      :initarg :x
      :initform nil
      :accessor y)
   (z :type float
      :initarg :x
      :initform nil
      :accessor z)
   (raw :type string
	:initarg :raw
	:initform nil
	:accessor raw))
  (:base-table "samples"))

(clsql:def-view-class Measurement ()
  ((id :db-kind :key
       :db-constraints :not-null
       :type integer
       :initarg :id
       :reader id)
   (name :type (clsql:varchar 255)
	 :initarg :name
	 :initform nil
	 :accessor name)
   (samples :db-kind :join
	    :db-info (:join-class Sample
		      :home-key id
		      :foreign-key measurement-id)
	    :accessor samples))
  (:base-table "measurements"))

(server-model:defmodel UserGroupSection ()
  ((id :db-kind :key
       :db-constraints :not-null
       :type integer
       :initarg :id
       :reader id)
   (name :type (clsql:varchar 255)
	 :initarg :name
	 :accessor name)
   (symbol :type (clsql:varchar 255)
	   :initarg :symbol
	   :accessor symbol)
   (description :type (string 32000)
		:initarg :description
		:accessor description)))


(server-url:defaction action-measurement-show (vars) ("measurements" :id)
  (server-db:with-default-database (db)
    (let* ((id (gethash "id" vars))
	   (m (car (clsql:select 'measurement :where [= [slot-value 'measurement 'id] id] :flatp t))))
      (with-page-layout (s)
	(:a :href (server-url:build-url-for "action-measurements") "Back")
	(:h1 (format s "Measurement: ~A" (name m)))
	(:h2 "Samples")
	(:ul
	 (loop for sample in (samples m) do
	      (cl-who:htm
	       (:li (cl-who:fmt "~A => (~A, ~A, ~A)" (id sample) (x sample) (y sample) (z sample))))))))))

(server-url:defaction action-measurements () ("measurements")
  (server-db:with-default-database (db)
    (let ((measurements (clsql:select 'measurement :flatp t)))
      (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
	(:table
	 (:th "Measurement")
	 (:th "Number of samples")
	 (dolist (m measurements)
	   (cl-who:htm 
	    (:tr
	     (:td :valign "top"
		  (:a :href (server-url:build-url-for "action-measurement-show" :id (id m)) (cl-who:str (name m))))
	     (:td (cl-who:str (length (samples m))))))))))))


(server-url:defaction user-group-section () ("usergroupsections")
    (with-page-layout (s)
      (server-model:scaffold-form s 'usergroupsection)))

