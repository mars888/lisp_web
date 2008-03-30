(in-package :color)

(defclass RGB (BaseColor)
  ((r :type long-float
      :initarg :r
      :initform 0
      :accessor r)
   (g :type long-float
      :initarg :g
      :initform 0
      :accessor g)
   (b :type long-float
      :initarg :b
      :initform 0
      :accessor b)))

(defmethod print-object ((rgb RGB) stream)
  (print-unreadable-object (rgb stream :type t :identity t)
    (format stream "r:~A g:~A b:~A" (r rgb) (g rgb) (b rgb))))

(defun make-rgb (&key (r 0) (g 0) (b 0))
  (make-instance 'rgb :r r :g g :b b))

(defmethod to-rgb ((rgb RGB))
  rgb)

