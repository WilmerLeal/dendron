;;;; package.lisp

(defpackage #:dendron
  (:use #:cl)
  (:export :dendron
	   :pattern-equal
	   :first-child
	   :second-child
	   :distance
	   :dendronp
	   :pattern-set
	   :elements))
