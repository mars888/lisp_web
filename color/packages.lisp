
(defpackage :color
  (:use :cl)
  (:export tristimulus-entry
	   make-tristimulus-entry
	   set-tristumulus-entry
	   get-tristimulus-entry
	   def-tristimulus-entry
	   list-tristimulus-entries
   
           base-color
	   to-lab
	   to-rgb

	   rgb
	   make-rgb

	   xyz
	   make-xyz

	   lab
	   make-lab))
