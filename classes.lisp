;-*-Lisp-*-

(in-package #:dendron)
;; Classes and some methods used in dendron.

(defclass pattern ()
  ((relaxed-support :accessor relaxed-support
		  :initform 0
		  :type float)
   (support :accessor support
	    :initform 0
	    :type float)
   (occurrence :accessor occurrence
	       :initform 0)
   (membership
    :accessor membership
    :initform (make-hash-table)))
  (:documentation
   "Root class for patterns."))

;; 
(defclass pattern-set (pattern)
  ((elements :accessor elements
	     :initarg :elements
	     :type list
	     :initform nil)
   (cardinal :accessor cardinal
	     :initarg :cardinal
	     :type integer))
  (:documentation "Class of patterns considered as leaves."))

(defclass dendron (pattern)
  ((first-child :accessor first-child
		  :initarg :first-child)
   (second-child :accessor second-child
		   :initarg :second-child)
   (distance :accessor distance
	     :initarg :distance
	     :type 'float)
   (associated-pattern-set :accessor pattern-set
			   :initarg :pattern-set)
   (ordinal :accessor ordinal
	    :type 'string))
  (:documentation "Class of patterns considered as trees."))

(defclass dendrogram ()
  ((root :accessor root
	 :initarg :root
	 :type 'dendron))
  (:documentation "Class possesing a root dendron."))

(defmacro dendronp (d)
  `(or (eq (type-of ,d) 'dendron)
       (subtypep (type-of ,d) 'dendron)))

;; ;;; Sort elements of a given pattern set (canonical form)
(defmethod initialize-instance :after ((ss pattern-set) &rest args)
  (declare (ignore args))
  (setf (elements ss) (sort (elements ss) *leaf-sorting-function*)
	(cardinal ss) (length (elements ss))))

(defmethod elements ((dd dendron))
  (elements (pattern-set dd)))

(defmethod (setf elements) (new-val (dd dendron))
  (setf (slot-value (slot-value dd 'pattern-set) 'elements) new-val))

;; Assign a new value to elements
(defmethod (setf elements) (new-val (ss pattern-set))
  (setf (slot-value ss 'elements) new-val
	(slot-value ss 'cardinal) (length new-val))
  (setf (slot-value ss 'elements)
	(sort (elements ss) *leaf-sorting-function*))
  new-val)

(defmethod elements ((dd dendrogram))
  (elements (root dd)))

;; Anything else
(defmethod elements (s)
  (list s))

(defmethod elements ((ss null))
  nil)

(defmethod leaves (s)
  (list s))

(defmethod cardinal ((dd dendron))
  (cardinal (pattern-set dd)))

(defmethod cardinal (ss)
  1)

(defmethod cardinal ((dd null))
  0)

(defmethod leaves ((dd dendron))
;; should work for non canonical dendrons
  (cond ((and (not (dendronp (first-child dd)))
	      (not (dendronp (second-child dd))))
	 (list (first-child dd) (second-child dd)))
	((not (dendronp (first-child dd)))
	 (cons (first-child dd) (leaves (second-child dd))))
	((not (dendronp (second-child dd)))
	 (cons (second-child dd) (leaves (first-child dd))))
	(t (append (leaves (first-child dd))
		   (leaves (second-child dd))))))

(defmethod leaves ((dd dendrogram))
  (leaves (root dd)))

(defmethod subdendrons ((dd dendrogram))
  (subdendrons (root dd)))

(defmethod enumerate ((dd dendrogram))
  (labels ((num (dd &optional (ord "r"))
	     (cond ((not (dendronp dd)))
		   (t (setf (ordinal dd) ord)
		      (num (first-child dd)
			   (concatenate 'string ord ".0"))
		      (num (second-child dd)
			   (concatenate 'string ord ".1"))))))
    (num (root dd))
    dd))

(defmethod pattern-equal ((d1 dendron) (d2 dendron))
  (cond ((or (not (eq (type-of (first-child d1))
		      (type-of (first-child d2))))
	     (not (eq (type-of (second-child d1))
		      (type-of (second-child d2))))
	     (not (equal (elements d1) (elements d2))))
	 nil)
	((and (not (dendronp (first-child d1)))
	      (not (dendronp (first-child d2))))
	 (and (eq (first-child d1) (first-child d2))
	      (eq (second-child d1) (second-child d2))))
	((and (not (dendronp (second-child d1)))
	      (not (dendronp (second-child d2))))
	 (and (eq (second-child d1) (second-child d2))
	      (pattern-equal (first-child d1) (first-child d2))))
	(t 
	 (and (pattern-equal (first-child d1) (first-child d2))
	      (pattern-equal (second-child d1) (second-child d2))))))

(defmethod pattern-equal ((s1 pattern-set) (s2 pattern-set))
  (equal (elements s1) (elements s2)))

(defmethod pattern-equal (s1 s2)
  (eq s1 s2))

;; The canonical form of a dendrogram is defined as
;;   - If FIRST-CHILD and SECOND-CHILD are both leaves, FIRST-CHILD and SECOND-CHILD
;;     are ordered alphabetically.
;;   - If one of them is not a leave, it must be SECOND-CHILD
;;   - Otherwise, FIRST-CHILD must be heavier than SECOND-CHILD.
;;   - If they are equally heavy, then are ordered alfabetically according
;;     to their elements.
(defmethod canonicalise ((dd dendron))
  (cond ((and (not (dendronp (first-child dd)))
	      (not (dendronp (second-child dd))))
	 (when (funcall *leaf-sorting-function*
			(second-child dd)
			(first-child dd))
	   (psetf (first-child dd) (second-child dd)
		  (second-child dd) (first-child dd))))
	((not (dendronp (second-child dd)))) ; do nothing
	((not (dendronp (first-child dd)))
	 (psetf (first-child dd) (second-child dd)
		(second-child dd) (first-child dd)))
	((< (length (leaves (first-child dd)))
	    (length (leaves (second-child dd))))
	 (psetf (first-child dd) (second-child dd)
		(second-child dd) (first-child dd)))
	((= (length (leaves (first-child dd)))
	    (length (leaves (second-child dd))))
	 (when (funcall *leaf-sorting-function*
			(car (leaves (second-child dd)))
			(car (leaves (first-child dd))))
	   (psetf (first-child dd) (second-child dd)
		  (second-child dd) (first-child dd))))))

(defmethod canonicalise (dd)
  (declare (ignore dd)))

;; find a leaf in elements of a pattern
(defmethod belongp (leaf (pp pattern))
  (find leaf (elements pp)))

;; Return a copy of a pattern set
(defmethod copy-pattern ((ss pattern-set))
  (make-instance 'pattern-set
		 :elements (copy-tree (elements ss))))

;; Return a new copy of dendron
(defmethod copy-pattern ((dd dendron))
  (cond ((and (not (dendronp (first-child dd)))
	      (not (dendronp (second-child dd))))
	 (make-instance 'dendron
			:first-child (first-child dd)
			:second-child (second-child dd)
			:pattern-set (copy-pattern (pattern-set dd))))
	((not (dendronp (second-child dd)))
	 (make-instance 'dendron
			:first-child (copy-pattern (first-child dd))
			:second-child (second-child dd)
			:pattern-set (copy-pattern (pattern-set dd))))
	(t (make-instance 'dendron
			  :first-child (copy-pattern (first-child dd))
			  :second-child (copy-pattern (second-child dd))
			  :pattern-set (copy-pattern (pattern-set dd))))))

;; Return a list of dendrons contained
;; in a given dendron. Leaves are omitted.
(defmethod subdendrons ((dd dendron))
  (cond ((and (not (dendronp (first-child dd)))
	      (not (dendronp (second-child dd))))
	 (list dd))
	((not (dendronp (first-child dd)))
	 (cons dd (subdendrons (second-child dd))))
	((not (dendronp (second-child dd)))
	 (cons dd (subdendrons (first-child dd))))
	(t (append (list dd)
		   (subdendrons (first-child dd))
		   (subdendrons (second-child dd))))))

;; Should work for non canonical dendrons
(defmethod mapdendron ((dd dendron) fn)
  (cond ((and (not (dendronp (first-child dd)))
	      (not (dendronp (second-child dd))))
	 (list (funcall fn dd)))
	((not (dendronp (first-child dd)))
	 (cons (funcall fn dd)
	       (mapdendron (second-child dd) fn)))
	((not (dendronp (second-child dd)))
	 (cons (funcall fn dd)
	       (mapdendron (first-child dd) fn)))
	(t (append (list (funcall fn dd))
		   (mapdendron (first-child dd) fn)
		   (mapdendron (second-child dd) fn)))))

(defmethod mapden ((dd dendron) fn)
  (cond ((and (not (dendronp (first-child dd)))
	      (not (dendronp (second-child dd))))
	 (funcall fn dd))
	((not (dendronp (first-child dd)))
	 (funcall fn dd)
	 (mapden (second-child dd) fn))
	((not (dendronp (second-child dd)))
	 (funcall fn dd)
	 (mapden (first-child dd) fn))
	(t (funcall fn dd)
	   (mapden (first-child dd) fn)
	   (mapden (second-child dd) fn))))

(defmethod dendrons-if ((dd dendron) pred)
  (if (funcall pred dd)
      (cond ((not (dendronp (first-child dd)))
	     (list dd))
	    ((not (dendronp (second-child dd)))
	     (cons dd (dendrons-if (first-child dd) pred)))
	    (t (append 
		(list dd)
		(dendrons-if (first-child dd) pred)
		(dendrons-if (second-child dd) pred))))
      (cond ((not (dendronp (first-child dd)))
	     nil)
	    ((not (dendronp (second-child dd)))
	     (dendrons-if (first-child dd) pred))
	    (t 
	     (append (dendrons-if (first-child dd) pred)
		     (dendrons-if (second-child dd) pred))))))

;; quick hack to find dendrons
(defmethod find-dendron ((dd dendron) (lst list))
  (let ((ll (elements-if #'(lambda (x) 
			     (equal (elements dd) 
				    (elements x)))
			 lst)))
    (find dd ll :test #'pattern-equal)))

;; Return a string corresponding to the printed
;; representation of a dendron (newick format)
(defmethod dendron-to-newick ((dd dendron) 
			      &key 
			      (leaf-print #'(lambda (x) (format nil "~a" x)))
			      (support #'support)
			      include-support)
  (cond ((not (dendronp (first-child dd)))
	 (format nil "(~a:~,6f,~a:~,6f)~:[~;~,6f~]" 
		 (funcall leaf-print (first-child dd))
		 (distance dd)
		 (funcall leaf-print (second-child dd))
		 (distance dd)
		 include-support
		 (funcall support dd)))
	((not (dendronp (second-child dd)))
	 (format nil "(~a:~,6f,~a:~,6f)~:[~;~,6f~]" 
		 (dendron-to-newick (first-child dd) 
				    :leaf-print leaf-print 
				    :support support
				    :include-support include-support)
		 (distance dd)
		 (funcall leaf-print (second-child dd))
		 (distance dd)
		 include-support
		 (funcall support dd)))
	(t
	 (format nil "(~a:~,6f,~a:~,6f)~:[~;~,6f~]"
		 (dendron-to-newick (first-child dd) 
				    :leaf-print leaf-print 
				    :support support
				    :include-support include-support)
		 (distance dd)
		 (dendron-to-newick (second-child dd) 
				    :leaf-print leaf-print 
				    :support support
				    :include-support include-support)
		 (distance dd)
		 include-support
		 (funcall support dd)))))

(defmethod membership-list ((dd dendrogram) (sb dendron))
  (loop for ee in (elements dd)
       collect (gethash ee (membership sb) 0)))
