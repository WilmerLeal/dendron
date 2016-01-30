
;;;; dendron.lisp
(in-package #:dendron)

(defun newick (lst)
  (cond ((not (consp lst)) lst)
	((= 4 (length lst))
	 (let (d1)
	   (destructuring-bind (fchild dist schild dist2) lst
	     (declare (ignore dist2))
	     (setf d1 (make-instance 'dendron
				     :first-child (newick fchild)
				     :second-child (newick schild)
				     :distance dist))
	       (setf (pattern-set d1)
		     (make-instance 'pattern-set
				    :elements (union (leaves (first-child d1))
						      (leaves (second-child d1))))))
	   (canonicalise d1)
	   d1))
	(t 
	   (error 'simple-error 
		  :format-string "Unlabeled Newick format should have exactly 4 arguments:~%~a, but has ~d arguments~%" 
		  :format-arguments (list lst (length lst))))))

(defun make-dendron (lst &key (format #'newick))
  "Return a dendron from list."
  (funcall format lst))

(defun make-dendrogram (lst &key (format #'newick))
  "Return a dendrogram from list"
  (enumerate (make-instance 'dendrogram :root (make-dendron lst :format format))))

(defmethod delete-leaves (leaves (ss pattern-set))
  (make-instance 'pattern-set :elements
		 (set-difference (elements ss) leaves)))

(defmethod delete-leaves (leaves (dd dendron))
  (let (d1)
    (cond ((every #'(lambda (x) (not (belongp x dd))) leaves)
	   dd)
	  ((every #'(lambda (x) (member x leaves)) (elements dd))
	    nil)
	  ;; from now on, dd is not completely contained in leaves
	  ((and (not (dendronp (first-child dd)))
		(not (dendronp (second-child dd))))
	   (if (member (first-child dd) leaves)
	       (second-child dd)
	       (first-child dd)))
	  ((not (dendronp (first-child dd)))
	   (cond ((member (first-child dd) leaves)
		  (delete-leaves leaves (second-child dd)))
		 ((every #'(lambda (x) (member x leaves)) (elements (second-child dd)))
		  (first-child dd))
		 (t (setf d1 (make-instance 'dendron :first-child (first-child dd)
					    :second-child (delete-leaves leaves (second-child dd))
					    :pattern-set (delete-leaves leaves (pattern-set dd))))
		    (canonicalise d1)
		    d1)))
	  ((not (dendronp (second-child dd)))
	   (cond ((member (second-child dd) leaves)
		  (delete-leaves leaves (first-child dd)))
		 ((every #'(lambda (x) (member x leaves)) (elements (first-child dd)))
		  (second-child dd))
		 (t (setf d1 (make-instance 'dendron 
					    :first-child (delete-leaves leaves (first-child dd))
					    :second-child (second-child dd)
					    :pattern-set (delete-leaves leaves (pattern-set dd))))
		    (canonicalise d1)
		    d1)))
	  ((every #'(lambda (x) (member x leaves)) (elements (first-child dd)))
	   (delete-leaves leaves (second-child dd)))
	  ((every #'(lambda (x) (member x leaves)) (elements (second-child dd)))
	   (delete-leaves leaves (first-child dd)))
	  (t
	   (setf d1 (make-instance 'dendron
				   :first-child (delete-leaves leaves (first-child dd))
				   :second-child (delete-leaves leaves (second-child dd))
				   :pattern-set (delete-leaves leaves (pattern-set dd))))
	   (canonicalise d1)
	   d1))))

;; Associate subdendrons to a given list of dendrons.
;; If a subdendron already exists in the list, its occurrence 
;; and that of its children are incremented.
(defmethod assoc-subdendrons ((dd dendron))
  (let ((d2 (find dd *dendrons* :test #'pattern-equal)))
    (cond (d2
	   (mapden d2 #'(lambda (x) (incf (support x)))))
	  (t
	   (incf (support dd))
	   (lpush dd *dendrons*)
	   (cond ((not (dendronp (first-child dd))))
		 ((not (dendronp (second-child dd)))
		  (assoc-subdendrons (first-child dd)))
		 (t
		  (assoc-subdendrons (first-child dd))
		  (assoc-subdendrons (second-child dd))))))))

(defmethod assoc-subdendrons ((dd dendrogram))
  (assoc-subdendrons (root dd)))

;; Associate pattern-set of a given dendron
;; to a list of patterns. If any pattern belongs
;; to that list, its occurrence is incremented.
(defmethod assoc-pattern-sets ((dd dendron))
  (let (val)
    (dolist (den (subdendrons dd))
      (setf val (find (pattern-set den) *pattern-sets* :test #'pattern-equal))
      (cond (val
	     (setf (pattern-set den) val)
	     (incf (support (pattern-set den))))
	    (t
	     (incf (support (pattern-set den)))
	     (push (pattern-set den) *pattern-sets*))))))

(defmethod containedp ((s1 pattern-set) (s2 pattern-set))
  (every #'(lambda (x) (member x (elements s2))) (elements s1)))

(defmethod containedp ((d1 dendron) (d2 dendron))
  (some #'(lambda (x) (pattern-equal d1 x)) (subdendrons d2)))

(defmethod containedp ((ss pattern-set) (dd dendron))
  (some #'(lambda (x) (pattern-equal ss x))
	(mapcar #'pattern-set (subdendrons dd))))

(defmethod containedp ((dd dendron) (ss pattern-set))
  (containedp (pattern-set dd) ss))

(defmethod containedp (leave (dd pattern))
  (member leave (elements dd)))

(defmethod similarity ((d1 dendron) (d2 dendron))
  (let* ((subd1 (subdendrons d1))
	(subd2 (subdendrons d2))
	(int (intersection subd1 subd2 :test #'pattern-equal)))
    (values
     (float (/ (length int)
	       (length (union subd1 subd2 :test #'pattern-equal))))
     int)))

(defmethod similarity ((ss pattern-set) (pp pattern))
  (let ((int (intersection (elements pp) (elements ss))))
    (values (/ (length int)
	       (length (union (elements pp) (elements ss))) 1.0)
	    int)))

(defmethod similarity ((ss pattern-set) (dd dendron))
  (similarity ss (pattern-set dd)))

(defmethod similarity ((dd dendron) (ss pattern-set))
  (similarity ss (pattern-set dd)))

(defmethod max-similarity ((pp pattern) (dd dendrogram))
  (let ((res (reduce #'(lambda (x y)
			 (if (> (car x) (car y)) x y))
		     (mapdendron (root dd)
				 #'(lambda (x)
				     (multiple-value-bind (val int)
							 (similarity pp x)
							 (cons val int)))))))
    (values (car res) (cdr res))))

(defmethod calc-membership ((pp pattern-set) int)
  (dolist (ee (elements pp))
    (when (member ee int)
      (incf (gethash ee (membership pp) 1)))))

(defmethod calc-membership ((dd dendron) int)
  (dolist (ee (subdendrons dd))
    (when (member ee int :test #'pattern-equal)
	  (incf (gethash ee (membership dd) 1)))))

(defmethod calc-membership ((pp pattern-set) (ii (eql :all)))
  (dolist (ee (elements pp))
    (incf (gethash ee (membership pp) 1))))

(defmethod calc-membership ((dd dendron) (ii (eql :all)))
  (dolist (ee (subdendrons dd))
    (incf (gethash ee (membership dd) 1))))

(defmethod calc-support ((pp pattern) (rr dendrogram) (method (eql :crispy)))
  (let* ((erased
	  (set-difference (elements pp)
			  (elements rr)))
	 (p1 (delete-leaves erased pp)))
    (when (or (null (elements p1))
	      (containedp p1 (root rr)))
      (incf (support pp)))))

(defmethod calc-support ((pp pattern) (rr dendrogram) (method (eql :relaxed)))
  (let* ((erased
	  (set-difference (elements pp)
			  (elements rr)))
	 (d1 (delete-leaves erased pp)))
    (if (> (length (elements d1)) 1)
	(multiple-value-bind (sim int)
	    (max-similarity d1 rr)
	  (incf (relaxed-support pp) sim)
	  (calc-membership pp int))
	(progn (incf (relaxed-support pp))
	       (calc-membership pp :all)))))

;;; Reading functions

(defun read-dend-from-string (str &key (format #'newick) (type 'dendrogram))
  "Read a tree structure from a string"
  (let ((line (copy-seq str))
	ss)
    (setf line (substitute #\SPACE #\: line))
    (setf line (substitute #\SPACE #\, line))
    (with-input-from-string (st line)
      (setf ss (read st)))
    (if (eq type 'dendrogram)
	(make-dendrogram ss :format format)
	(make-dendron ss :format format))))

(defun read-pset-from-string (str)
  (let (ss)
    (with-input-from-string (st str)
      (setf ss (read st)))
    (make-instance 'pattern-set :elements ss)))

(defmacro do-lines ((var file &optional (counter (gensym))) &body body)
  `(handler-case
       (with-open-file (thy ,file)
	 (setf ,counter 0)
	 (loop for ,var = (read-line thy nil) then (read-line thy nil)
	    until (null ,var)
	    do
	      ,@body
	      (incf ,counter)))
     (end-of-file nil
       (error 'simple-error
	      :format-control "Incomplete data in file at line: ~d."
	      :format-arguments (list ,file ,counter)))
     (simple-error (cond)
       (error 'simple-error
	      :format-control "There is an error in the file: ~a, at line: ~d.~%~a"
	      :format-arguments (list ,file ,counter cond)))
     (file-error (cond)
       (error 'simple-error
	      :format-control "Some error occured when reading file `~a' at line ~d~%~a" ,file ,counter cond))))

(defun read-patterns-from-file (file &key
				(format #'newick)
				(dendron-as :both)
				include-subdendrons)
  (let (patterns dd (cc 0))
    (do-lines (line file cc)
      (cond ((or (> (count #\( line :test #'char=) 1)
		 (position #\: line))
	     (setf dd (make-dendron (read-dend-from-string line :format format :type 'dendron)))
	     (cond ((eq dendron-as :graph)
		    (if include-subdendrons
			(mapden dd #'(lambda (x)
				     (lpush x patterns)))
			(lpush dd patterns)))
		   ((eq dendron-as :set)
		    (if include-subdendrons
			(mapden dd #'(lambda (x)
				       (lpush (pattern-set x) patterns)))
			(lpush (pattern-set dd) patterns)))
		   (t
		    (if include-subdendrons
			(mapden dd #'(lambda (x)
				       (lpush x patterns)
				       (lpush (pattern-set x) patterns)))
			(progn (lpush dd patterns)
			       (lpush (pattern-set dd) patterns))))))
	    ((= (count #\( line :test #'char=) 1)
	     (lpush (read-pset-from-string line) patterns))
	    (t (warn "Pattern in file ~a at line ~d is not a set nor a dendron, not taken into account" file cc))))
    patterns))

(defun support-patterns (patfile repfile &key
			 (method :all)
			 (format #'newick)
			 (include-subdendrons nil)
			 (dendron-as :both))
  (let ((patterns (read-patterns-from-file patfile 
					   :format format 
					   :dendron-as dendron-as 
					   :include-subdendrons include-subdendrons))
	rr cc)
    (do-lines (ll repfile cc)
      (setf rr (read-dend-from-string ll))
      (dolist (pp patterns)
	(cond ((and (eq method :all)
		    (eq (type-of pp) 'pattern-set))
	       (calc-support pp rr :crispy)
	       (calc-support pp rr :relaxed))
	      ((eq (type-of pp) 'pattern-set)
	       (calc-support pp rr method))
	      ((eq method :all)
	       (calc-support pp rr :crispy)
	       (calc-support pp rr :relaxed))
	      (t (calc-support pp rr method)))))
    (mapc #'(lambda(x)
	      (setf (support x)
		    (/ (support x) cc)
		    (relaxed-support x)
		    (/ (relaxed-support x) cc)))
	  patterns)
  patterns))

(defun print-pattern-list (plist out &key (fs #\TAB) (support :all))
  (setf *print-pretty* nil)
  (cond ((eq support :all)
	 (format out "pattern~Ccrispy~Crelaxed~%" fs fs))
	((eq support :crispy)
	 (format out "pattern~Ccrispy~%" fs))
	(t
	 (format out "pattern~Crelaxed~%" fs)))
  (dolist (pp plist)
    (cond ((and (eq support :all)
		(eq (type-of pp) 'dendron))
	   (format out "~a;~C~,4f~C~,4f~%" (dendron-to-newick pp) fs (support pp) fs (relaxed-support pp)))
	  ((and (dendronp pp)
		(eq support :crispy))
	   (format out "~a;~C~,4f~%" (dendron-to-newick pp) fs (support pp)))
	  ((dendronp pp)
	   (format out "~a;~C~,4f~%" (dendron-to-newick pp) fs (relaxed-support pp)))
	  ((eq support :all)
	   (format out "~S;~C~,4f~C~,4f~%" (elements pp) fs (support pp) fs (relaxed-support pp)))
	  ((eq support :crispy)
	   (format out "~S;~C~,4f~%" (elements pp) fs (support pp)))
	  (t (format out "~S;~C~,4f~%" (elements pp) fs (relaxed-support pp))))))
