(in-package :color)

(defstruct tristimulus-entry
  "Describes a tristimulus entry. Used for transforming XYZ into Lab."
  (name ""    :type string)
  (note "N/A" :type string)
  (x-ref 0.0  :type float)
  (y-ref 0.0  :type float)
  (z-ref 0.0  :type float))


(defvar *tristimulus-entries* (make-hash-table :test 'equal))

(defun set-tristimulus-entry (name entry)
  (setf (gethash (symbol-name name) *tristimulus-entries*) entry))

(defun get-tristimulus-entry (name)
  (gethash (symbol-name name) *tristimulus-entries*))

(defun list-tristimulus-entries ()
  (loop for k being the hash-keys of *tristimulus-entries* do
       (format t "~A(~A) => ~A~%" k (type-of k) (gethash k *tristimulus-entries*))))


(defmacro def-tristimulus-entry (name &body body)
  `(set-tristimulus-entry ',name (make-tristimulus-entry ,@body)))

(def-tristimulus-entry d50-2deg
  :name  "D50 2Deg"
  :x-ref 96.422
  :y-ref 100.0
  :z-ref 82.521)

(def-tristimulus-entry d50-10deg
  :name  "d50 10Deg"
  :x-ref 96.720
  :y-ref 100.0
  :z-ref 81.427)

(def-tristimulus-entry d55-2deg
  :name  "d55 2Deg"
  :x-ref 95.682
  :y-ref 100.0
  :z-ref 92.149)

(def-tristimulus-entry d65-2deg
  :name  "d65 2Deg"
  :note  "Television, sRGB color space"
  :x-ref 95.047
  :y-ref 100.0
  :z-ref 108.883)

(def-tristimulus-entry d65-10deg
  :name  "d65 10Deg"
  :x-ref 94.811
  :y-ref 100.0
  :z-ref 107.304)


