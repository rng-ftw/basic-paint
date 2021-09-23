(defproject paint "0.1.0-SNAPSHOT"
	:description "basic paint style program"
	:dependencies [[org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]
				 [vlaaad/reveal "1.3.212"]
				 [cider/nrepl "0.3.0"]
				 ]
	:main paint.core	
	:plugins [[lein-cljfmt "0.8.0"]]	
)
