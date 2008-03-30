(in-package :color)

(defclass BaseColor ()
  ())

(defgeneric to-lab (base-color &optional trireference-name)
  (:documentation "Convert passed color to LAB"))
(defmethod to-lab ((base-color BaseColor) &optional trireference-name)
  (error "Converting to LAB not supported for this type."))

(defgeneric to-rgb (base-color)
  (:documentation "Convert passed color to RGB"))
(defmethod to-rgb ((base-color BaseColor))
  (error "Converting to RGB not supported for this type."))


