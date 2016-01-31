;;;; dendron.asd

(asdf:defsystem #:dendron
  :serial t
  :description "Dendron is simple tool for calculating statistical support of dendrograms"
  :author "Eugenio J. Llanos <ellanos@sciocorp.org> and Wilmer Leal <wilmerlealj@gmail.com>"
  :license "This product is realeased under the terms of GPL v3.0"
  :components ((:file "package")
	       (:file "variables" :depends-on ("package"))
	       (:file "auxiliary" :depends-on ("package"))
	       (:file "classes" :depends-on ("package" "auxiliary" "variables"))
               (:file "dendron" :depends-on ("package" "auxiliary" "variables" "classes"))
	       (:file "random-sampler" :depends-on ("package" "auxiliary" "variables"))))
