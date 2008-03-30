(in-package :color)

(defclass LAB (BaseColor)
  ((l :type long-float
      :initarg :l
      :initform 0
      :accessor l)
   (a :type long-float
      :initarg :a
      :initform 0
      :accessor a)
   (b :type long-float
      :initarg :b
      :initform 0
      :accessor b)))

(defmethod print-object ((lab LAB) stream)
  (print-unreadable-object (lab stream :identity t :type t)
    (format stream "l:~A a:~A b:~A" (l lab) (a lab) (b lab))))

(defun make-lab (&key (l 0) (a 0) (b 0))
  (make-instance 'lab :l l :a a :b b))


