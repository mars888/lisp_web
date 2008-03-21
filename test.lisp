

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


