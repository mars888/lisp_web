
(defsystem "color"
    :version 0.4
    :components ((:module "color"
			  :components ((:file "packages")
				       (:file "tristimulus" :depends-on ("packages"))
				       (:file "base-color"  :depends-on ("packages"))
				       (:file "rgb"         :depends-on ("packages" "base-color"))
				       (:file "xyz"         :depends-on ("packages" "base-color" "tristimulus"))
				       (:file "lab"         :depends-on ("packages" "base-color"))))))
