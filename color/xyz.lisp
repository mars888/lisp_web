(in-package :color)

(defclass XYZ (BaseColor)
  ((x :type long-float
      :initarg :x
      :initform 0
      :accessor x)
   (y :type long-float
      :initarg :y
      :initform 0
      :accessor y)
   (z :type long-float
      :initarg :z
      :initform 0
      :accessor z)))

(defmethod print-object ((xyz XYZ) stream)
  (print-unreadable-object (xyz stream :type t :identity t)
    (format stream "x:~A y:~A z:~A" (x xyz) (y xyz) (z xyz))))

(defun make-xyz (&key (x 0) (y 0) (z 0))
  (make-instance 'xyz :x x :y y :z z))

(defun clamp (value min max)
  (if (< value min)
      (setf value min))
  (if (> value max)
      (setf value max))
  value)

(defmethod to-rgb ((xyz XYZ))
  (let* ((var-x (/ (x xyz) 100))
	 (var-y (/ (y xyz) 100))
	 (var-z (/ (z xyz) 100))
	 (var-r (+ (* var-x 3.2406)
		   (* var-y -1.5372)
		   (* var-z -0.4986)))
	 (var-g (+ (* var-x -0.9689)
		   (* var-y 1.8758)
		   (* var-z 0.0415)))
	 (var-b (+ (* var-x 0.0557)
		   (* var-y 1.8758)
		   (* var-z 1.0570))))
    (if (> var-r 0.0031308)
	(setf var-r (- (* 1.055 (expt var-r (/ 1 2.4))) 0.055))
	(setf var-r (* 12.92 var-r)))
    (if (> var-g 0.0031308)
	(setf var-g (- (* 1.055 (expt var-g (/ 1 2.4))) 0.055))
	(setf var-g (* 12.92 var-g)))
    (if (> var-b 0.0031308)
	(setf var-b (- (* 1.055 (expt var-b (/ 1 2.4))) 0.055))
	(setf var-b (* 12.92 var-b)))
    (let ((r (clamp (* var-r 255) 0 255))
	  (g (clamp (* var-g 255) 0 255))
	  (b (clamp (* var-b 255) 0 255)))
      (make-rgb :r r :g g :b b))))


(defmethod to-lab ((xyz XYZ) &optional (trireference-name 'd50-2deg))
  (let ((tristim (get-tristimulus-entry trireference-name)))
    (unless tristim
      (error "Defined tristimulus reference is not valid."))
    (let* ((var-x (/ (x xyz) (tristimulus-entry-x-ref tristim)))
	   (var-y (/ (y xyz) (tristimulus-entry-y-ref tristim)))
	   (var-z (/ (z xyz) (tristimulus-entry-z-ref tristim))))
      (setf var-x (if (> var-x 0.008856) 
		      (expt var-x (/ 1.0 3.0))
		      (+ (* 7.787 var-x) (/ 16.0 116.0)))
	    var-y (if (> var-y 0.008856)
		      (expt var-y (/ 1.0 3.0))
		      (+ (* 7.787 var-y) (/ 16.0 116.0)))
	    var-z (if (> var-z 0.008856)
		      (expt var-z (/ 1.0 3.0))
		      (+ (* 7.787 var-z) (/ 16.0 116.0))))
      (let ((l (- (* 116.0 var-y) 16.0))
	    (a (* 500.0 (- var-x var-y)))
	    (b (* 200.0 (- var-y var-z))))
	(make-lab :l l :a a :b b)))))


