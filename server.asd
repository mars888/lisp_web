

(defsystem server
    :name "server"
    :version "0.0.1"
    :depends-on ("hunchentoot" "split-sequence" "cl-who")
    :components ((:module "server"
			  :components ((:file "packages")
				       (:file "url"     :depends-on ("packages"))
				       (:file "control" :depends-on ("packages" "url"))))))