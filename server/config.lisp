(in-package :server-config)


(defstruct config
  (items (make-hash-table :test 'equal) :type hash-table))

(defconstant +default-config-name+ "config.ini")
(defvar *config-loaded* nil "Is a config loaded?")
(defvar *config* (make-config))

(defun config-loaded-p ()
  *config-loaded*)

(defun load-default-config ()
  (load-config +default-config-name+))

(defun load-config (filename)
  (let ((config (make-config)))
    (with-open-file (s filename)
      ;; Walk lines getting only those that are not empty and that do not start with a '#'.
      (loop for line = (read-line s nil 'eof) 
	 until (eq line 'eof)
	 when (not (equal line ""))
	 when (not (eq (char line 0) #\#)) 
	 do
	   (let* ((split-point (position #\Space line))
		  (key (subseq line 0 split-point))
		  (value (if (or (null split-point) 
				 (= split-point (length line))) 
			     nil
			     (subseq line (1+ split-point)))))
	     ;; (format t "~A => ~A~%" key value)
	     (setf (gethash key (config-items config)) value))))
    (setf *config-loaded* t)
    (setf *config* config)))

(defun print-config ()
  (let ((items (config-items *config*)))
    (loop for key being the hash-keys in items do
	 (format t "\"~A\" => \"~A\"~%" key (gethash key items)))))

(defun has-item (key)
  (multiple-value-bind (value present-p) (get-item key)
    (declare (ignore value))
    present-p))

(defun get-item (key)
  (gethash key (config-items *config*)))

;; Load config on load.
;; (eval-when (:load-toplevel :compile-toplevel)
;;   (if (and (functionp 'load-config)
;; 	   (probe-file "config.ini"))
;;       (load-config "config.ini")))
; (load-config "config.ini") 