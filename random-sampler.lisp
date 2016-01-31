;-*-lisp-mode-*-

;; Routines to extract replicas from
;; a distance matrix.
(in-package :dendron)

(defclass nmatrix ()
  ((item :accessor matrix
	 :initarg :matrix
	 :type 'array)
   (labels :accessor mlabels
	   :initarg :mlabels)
   (indexes-hash-table
    :accessor indexes
    :initarg :indexes
    :initform (make-hash-table :test #'equal)
    :type 'hash-table)))

(defmethod initialize-instance :after ((nm nmatrix) &rest args)
  (declare (ignore args))
  (let ((lbls (mlabels nm)))
    (dotimes (ll (length lbls))
      (setf (gethash (nth ll lbls) (indexes nm))
	    ll))))

(defun make-nmatrix (lbls &key initial-contents)
  (let (content)
    (cond ((consp initial-contents)
	   (setf content initial-contents))
	  ((eq initial-contents :random)
	   (dotimes (i (length lbls))
	     (lpush 
	      (loop for j below (length lbls)
		 collect (random 1.0))
	      content))))
    (make-instance 'nmatrix
		   :mlabels lbls
		   :matrix (if initial-contents
			       (make-array (list (length lbls) (length lbls))
					   :initial-contents content)
			       (make-array (list (length lbls) (length lbls)))))))
  
(defgeneric maref (nm s1 s2)
  (:documentation "Accessor method for matrix elements."))

(defmethod maref ((nm nmatrix) s1 s2)
  (cond ((and (gethash s1 (indexes nm))
	      (gethash s2 (indexes nm)))
	 (aref (matrix nm)
	       (gethash s1 (indexes nm))
	       (gethash s2 (indexes nm))))
	(t (error 'simple-error
		  :format-control "Element not found in matrix' labels: '~a' or '~a'"
		  :format-arguments (list s1 s2)))))

(defmethod (setf maref) (new-val (nm nmatrix) s1 s2)
  (cond ((and (gethash s1 (indexes nm))
	      (gethash s2 (indexes nm)))
	 (setf (aref (matrix nm)
		     (gethash s1 (indexes nm))
		     (gethash s2 (indexes nm)))
	       new-val))
	(t (error 'simple-error
		  :format-control "Element not found in matrix' labels: '~a' or '~a'"
		  :format-arguments (list s1 s2)))))

(defmethod marray-dimensions ((nm nmatrix))
  (array-dimensions (matrix nm)))

(defun read-nmatrix-from-file (file &optional (fs #\space))
  (labels ((comp-0 (lst ll)
	     (let ((l2 (copy-tree lst)))
	       (dotimes (i (- ll (length lst)))
		 (lpush 0 l2))
	       l2)))
    (let (line res labels le)
      (with-open-file (in file :direction :input)
	(setf labels (split-string (read-line in) fs))
	(setf le (length labels))
	(dotimes (i le)
	  (setf line (concatenate 'string "(" (read-line in) ")"))
	  (lpush (comp-0 (cdr (read-from-string line)) le)
		 res)))
      (make-nmatrix labels :initial-contents res))))

(defmethod print-nmatrix ((nm nmatrix) &optional (out t) (triangular t))
  (let (line)
    (format out " ~{~a~,^ ~}~%" (mlabels nm))
    (loop for i below (array-dimension (matrix nm) 0)
	 do
	 (setf line
	       (loop for j below (if triangular i (array-dimension (matrix nm) 1))
		  collect (aref (matrix nm) i j)))
	 (format out "~a ~{~a~,^ ~}~%" (nth i (mlabels nm)) line))))

(defmethod generate-replica ((nm nmatrix) &optional (frac 0.05))
  (when (not (numberp frac))
    (error 'type-error 
	   :format-control "The value: ~a is not of type number."
	   :format-arguments (list frac)))
  (let ((cc (floor (* frac (array-dimension (matrix nm) 0))))
	deleted res dd ll tt)
    (declare (dynamic-extent res ll dd ll tt deleted))
    (do ((i 0))
	((>= i cc))
      (setf dd (random (array-dimension (matrix nm) 0)))
      (when (not (member dd deleted))
	(incf i)
	(push dd deleted)))
    (setf dd (delete-if #'(lambda (x)
			    (member (gethash x (indexes nm)) deleted))
			(copy-tree (mlabels nm))))
    (dotimes (i (array-dimension (matrix nm) 0))
      (setf tt nil)
      (when (not (member i deleted))
	(dotimes (j (array-dimension (matrix nm) 0))
	  (when (not (member j deleted))
	    (lpush (aref (matrix nm) i j) tt)))
	(lpush tt ll)))
    (setf res (make-nmatrix dd :initial-contents ll))))
    
