(in-package server-url)

(defvar *registered-urls* (make-hash-table :test 'equal))

(defclass URL ()
  ((name    :initform nil
	    :initarg :name
	    :accessor name)
   (handler :initform nil
	    :initarg :handler
	    :accessor handler)
   (path    :initform nil
	    :initarg :path
	    :accessor path)))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (format stream "name: ~A  path: ~A  handler: ~A" (name url) (path url) (handler url))))


(defun make-url (&key name handler path)
  (let ((instance (make-instance 'URL :name name :handler handler :path path)))
    (if name
	(register-url instance))
    instance))


; (defvar *url* (make-url :handler 'foo :path '("books" "show" :id)))

(defun register-url (url)
  (declare (type url url))
  (setf (gethash (name url) *registered-urls*) url))

(defun unregister-url (name)
  (remhash name *registered-urls*))

(defun clear-registered-urls ()
  (clrhash *registered-urls*))

(defun show-registered-urls ()
  (loop for url being the hash-values of *registered-urls* do
    (format t "~A~%" url)))

(defun get-registered-url (name)
  (gethash name *registered-urls*))



(defmethod match ((url URL) (request-path string))
  "Try to match the passed url object with the passes path string. If it matches, a hash with matches key items is
   returned. If it does not match, nil is returned."
  (labels ((match-parts (url-parts request-parts result-hash)
	     (if (or (null url-parts) (null request-parts)) ; Either nil
		 (if (not (and (null url-parts) (null request-parts))) ; But not both nil
		     nil
		     result-hash)
		 ; Else continue checking next part.
		 (let ((first-url  (car url-parts))
		       (first-path (car request-parts)))
		   (typecase first-url
		     (string (if (equal first-url first-path)
				 (match-parts (cdr url-parts) (cdr request-parts) result-hash)
				 nil))
		     (keyword (let ((keyword-string (string-downcase (symbol-name first-url))))
				(setf (gethash keyword-string result-hash) first-path)
				(match-parts (cdr url-parts) (cdr request-parts) result-hash)))
		     (t (format t "WARNING: Don't know how to match ~A~%" (type-of first-url))))))))
    (let ((url-path   (copy-list (path url)))
	  (path-parts (split-sequence:split-sequence #\/ request-path))
	  (result-hash (make-hash-table :test 'equal)))
      (if (equal (car path-parts) "")
	  (pop path-parts))
      (match-parts url-path path-parts result-hash))))

(defun find-match-for (path)
  "Find a url for the given path, returns nil if no match could be found."
  (loop for url being the hash-values of *registered-urls* do
       (let ((match (match url path)))
	 (if match
	     (return-from find-match-for (values url match))))))

(defmethod build-url ((url url) args)
  "Build a url string for the given url name and given args. Args should be a list.
   Example:
   (build-url (make-url :name 'foo' :handler 'foo :path '('books' 'show' :id)) '(:id 10)) => /books/show/10"
  (labels ((build-url-r (parts so-far)
	     (if (null parts)
		 so-far
		 (let ((first-part (car parts)))
		   (typecase first-part
		     (string  (push first-part so-far)
			      (build-url-r (cdr parts) so-far))
		     (keyword (push (getf args first-part) so-far)
			      (build-url-r (cdr parts) so-far))
		     (t (format t "Don't know how to handle something of type ~A~%" (type-of first-part)))))))
	   (combine-to-path (items stream)
	     ; Basically this is a normal join function.
	     (when items
	       (format stream "/")
	       (format stream "~A" (car items))
	       (combine-to-path (cdr items) stream))))
    (let* ((url-parts (path url))
	   (result (build-url-r url-parts nil)))
      (if result
	  (with-output-to-string (s)
	    (combine-to-path (nreverse result) s))))))

(defun build-url-for (name &rest args)
  "Same as build-url, but add url lookup by name."
  (let ((url (get-registered-url name)))
    (unless url
      (error (format nil "No URL with name ~A" name)))
    (build-url url args)))


(defmacro defaction (name (&optional (var-name (gensym "varname") var-name-given-p)) url-parts &body body)
  `(progn (defun ,name (,var-name)
	    ,(unless var-name-given-p 
		     `(declare (ignore ,var-name)))
	    ,@body)
	  (register-url (make-url :name ,(string-downcase (symbol-name name)) :handler ',name :path ',url-parts))))


(defaction registered-urls () ("registered-urls")
  (cl-who:with-html-output-to-string (s nil :indent t)
    (:table
     (:tr
      (:th "Name")
      (:th "Handler")
      (:th "Path")
      (:th "Unregister"))
     (loop for url being the hash-values of *registered-urls* do
	  (cl-who:htm
	   (:tr
	    (:td (cl-who:str (name url)))
	    (:td (cl-who:str (handler url)))
	    (:td (cl-who:str (path url)))
	    (:td (:a :href (build-url-for "do-unregister-url" :name (name url)) "Unregister"))))))))

(defaction registered-urls-name (vars) ("registered-urls" :name)
  (let* ((name (gethash "name" vars))
	 (url  (gethash name *registered-urls*)))
    (cl-who:with-html-output-to-string (s)
      (if url
	  (cl-who:htm "Found url " (:strong (cl-who:str (name url))))
	  (format s "No URL found with name ~A" name)))))

(defaction do-unregister-url (vars) ("unregister-url" :name)
  (unregister-url (gethash "name" vars))
  (cl-who:with-html-output-to-string (s)
    "URL " (cl-who:str (gethash "name" vars)) " unregistered. Go " (:a :href (build-url-for "registered-urls") "back")))


