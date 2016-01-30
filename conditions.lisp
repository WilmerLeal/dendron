;-*-Lisp mode-*-

;; Contains error conditions
;; of dendron

(in-package :dendron)

(define-condition malformed-file (error)
  ((file :accessor file
	 :initarg :file)
   (message :accessor message
	    :initarg :message)))

(define-condition malformed-line (malformed-file)
  ((line :accessor line
	 :initarg :line)))

(define-condition invalid-format (error)
  ((format :accessor current-format
	   :initarg :current-format)))
