;-*-Lisp-*-

(in-package #:dendron)
;; Global variables used by dendron

(defvar *dendrons* nil "A list containing all dendrons read.")

(defvar *pattern-sets* nil "A list containing all pattern sets read.")

(defvar *input-formats* nil "An alist containing formats and tree parsing functions.")

(defvar *leaf-sorting-function* #'(lambda (x y) 
				    (if (and (numberp x) (numberp y)) (< x y)
					(string< (symbol-name x) (symbol-name y))))
  "Function to sort leaves during canonicalisation.")

(defun reset-vars ()
  (setf *dendrons* nil
	*pattern-sets* nil))
