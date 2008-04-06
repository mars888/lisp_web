

(defsystem server
    :name "server"
    :version "0.0.1"
    :depends-on ("hunchentoot"
		 "split-sequence"
		 "cl-who"
		 "clsql"
		 "arnesi")
    :components ((:module "server"
			  :components ((:file "packages")
				       (:file "config"   :depends-on ("packages"))
				       (:file "url"      :depends-on ("packages"))
				       (:file "db"       :depends-on ("packages" "config"))
				       (:file "control"  :depends-on ("packages" "url" "config"))
				       (:file "model"    :depends-on ("packages" "db"))
				       (:file "scaffold" :depends-on ("packages" "db" "model" "url"))))))