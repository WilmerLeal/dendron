; -*-mode: Lisp-*-

;; Contains some auxiliary functions
;; and macros

(in-package #:dendron)

;; Push at the end of a list
(defmacro lpush (obj lst)
  "Replace cdr of the last cons with a cons containing an object as its car."
  `(cond ((null ,lst) (setf ,lst (list ,obj)))
     (t (push ,obj (rest (last ,lst))))))

(defun elements-if (pred lst &optional res)
  "Return elements of a list obeying a given predicate."
  (cond ((null lst) res)
	((funcall pred (car lst))
	 (elements-if pred (cdr lst) (cons (car lst) res)))
	(t (elements-if pred (cdr lst) res))))

(defun minimum (lst &key (key #'identity) (min most-positive-fixnum) res)
  (cond ((null lst) (values res min))
	((< (funcall key (car lst)) min)
	 (minimum (cdr lst) :key key :min (funcall key (car lst)) :res (car lst)))
	(t (minimum (cdr lst) :key key :min min :res res))))

(defun maximum (lst &key (key #'identity) (max most-positive-fixnum) res)
  (cond ((null lst) (values res max))
	((> (funcall key (car lst)) max)
	 (maximum (cdr lst) :key key :max (funcall key (car lst)) :res (car lst)))
	(t (maximum (cdr lst) :key key :max max :res res))))

(defun split-string (str &optional (fs #\SPACE) empty)
  (let (res (dd ""))
    (dotimes (i (length str))
      (cond ((and empty
		  (char= (elt str i) fs))
	     (lpush dd res)
	     (setf dd ""))
	    ((char= (elt str i) fs)
	     (when (not (string= dd ""))
	       (lpush dd res)
	       (setf dd "")))
	    (t (setf dd (concatenate 'string dd
				     (list (elt str i)))))))
    (when (or (and empty
		   (string= "" dd))
	      (string/= "" dd))
      (lpush dd res))
    res))
