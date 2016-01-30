;-*-Lisp mode-*-

;; Use this file for building
;; an executable with buildapp

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'dendron)
(in-package :dendron)

(defvar *dendron-usage*
  "dendron -- a simple tool for calculating frequency of patterns within dendrograms. Such calculation is performed
              using input patterns, stored in a file, and a set of dendrograms (replicas), stored in other file using
              newick format. dendron's output is a table having input patterns and their frequencies.

              Input patterns may be sets, dendrograms or branches (dendrons). Such patterns are compared with those ones
              coming from the set of replicas using two methods: crispy and relaxed. Using 'crispy' method dendron search
              for exact matches of the given patterns throughout the replicas. When relaxed method is selected, then
              a relaxed measure of similarity is used for finding the most similar subpattern within each replica.

 Usage:      dendron --patterns FILE --replicas REP_FILE [--output PREFIX] [--method METHOD] [--include-subdendrons] [--dendrons-as TYPE]

   --dendron-as TYPE         When a dendron is found in the pattern file, use TYPE
                             for comparison. TYPE might be one of the following values:
                             'set', 'graph' or 'both'. If the value of TYPE is 'set',
                             then dendrons are treated as sets. If TYPE is 'graph', then
                             they are treated as graphs (compare as drawings). If TYPE
                             is 'both' (default), then frequencies are calculated both as sets 
                             and as graphs.

   --help                    Display this text.

   --include-subdendrons     Recursively calculate frequency of subdendrons of each dendron 
                             in FILE. Useful for calculating the frequency of a whole dendrogram 
                             and all its branches.
                             Might be time consuming for large dendrograms, especially using
                             graph comparison.

   --method METHOD           METHOD may be one of:
                             'crispy', calculate frequency of patterns
                              using the crispy comparison.

                             'relaxed', calculate frequency of patterns
                              using a relaxed measure.

                             'all', calculate frequency of patterns using all
                              measures (default).

   --output OUTPUT           OUTPUT is the name of the output file. It stores all
                             input patterns (see --paterns) and their corresponding 
                             frequency in a table. Fields are separated by a TAB character.

   --patterns FILE           Patterns are read from file FILE, one per line.
                             Patterns might be of the type `set' or `dendron'. 
                             Sets are lists of elements separated by spaces and
                             enclosed by `(' and `)'. Dendrons are newick formated
                             dendrograms or branches (see examples). Elements should
                             not have any of the following characters: SPACE, TAB,
                             LINEFEED, CARRIAGE_RETURN, COLON, COMMA, SEMICOLON,
                             APOSTROPHE and DOUBLE_QUOTES.

   --replicas REP_FILE       Read REP_FILE to load dendrograms where to calculate frequency of patterns.
                             One dendrogram per line in newick format.
                             
 Examples:   dendron --patterns example.tre --output example-output --method crispy-set --replicas rep.tre
             To calculate supports of patterns in example.tre using a crispy set comparison whith dendrograms
             from the file rep.tre.

             dendron --replicas rep.tre --patterns example2.tre --output example2-out --method all --include-subdendrons
             To calculate supports of patterns in the file example.tre, including their branches and using all methods."
  "A string explaining how dendron works.")

(defvar *pattern-file* nil "File where patterns are input")
(defvar *output-file* "dendron-output" "The prefix name for the output file.")
(defvar *include-subdendrons* nil "A flag for including subdendrons in the calculations.")
(defvar *dendron-as* :both)
(defvar *method* :all)
(defvar *patterns* nil)
(defvar *replicas* nil)

(defun dendron-quit (&optional (code 0))
  "Portable quit."
   #+allegro (excl:exit code)
   #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
   #+cmu (ext:quit code)
   #+sbcl (sb-ext:quit :unix-status code)
   #+gcl (lisp:bye code))

(defun support-patterns-from-file  (argv)
  (handler-case
      (progn (cond ((member "--help" argv :test #'string=)
		    (format t "~a~%" *dendron-usage*)
		    (dendron-quit))
		   (t (do ((arg argv (cdr arg)))
			  ((null arg))
			(cond ((string= "--patterns" (car arg))
			       (setf arg (cdr arg)
				     *pattern-file* (pathname (car arg))))
			      ((string= "--include-subdendrons" (car arg))
			       (setf *include-subdendrons* t))
			      ((string= "--replicas" (car arg))
			       (setf arg (cdr arg)
				     *replicas* (pathname (car arg))))
			      ((string= "--output" (car arg))
			       (setf arg (cdr arg)
				     *output-file* (car arg)))
			      ((string= "--method" (car arg))
			       (setf arg (cdr arg)
				     *method* (intern (string-upcase (car arg)) :keyword)))
			      ((string= "--dendron-as" (car arg))
			       (setf arg (cdr arg))
			       (setf *dendron-as* (intern (string-upcase (car arg)) :keyword)))))))
	     (when (not (member *dendron-as* '(:graph :set :both)))
	       (error 'simple-error
		      :format-control "option dendron-as: '~a' not recognized" 
		      :format-arguments (list (symbol-name *dendron-as*))))
	     (when (not (member *method* '(:relaxed :crispy :all)))
	       (error 'simple-error
		      :format-control "option method: '~a' not recognized" 
		      :format-arguments (list (symbol-name *method*))))
	     (when (or (not *pattern-file*)
		       (not (directory *pattern-file*)))
	       (error 'simple-error
		      :format-control "no pattern file given or does not exist"))
	     (when (or (not *replicas*)
		       (not (directory *replicas*)))
	       (error 'simple-error
		      :format-control "no replica file given or does not exist"))
	     (setf *patterns* (support-patterns *pattern-file* *replicas*
						:method (intern (string-upcase *method*) :keyword)
						:include-subdendrons *include-subdendrons*
						:dendron-as *dendron-as*))
	     (handler-case
		 (with-open-file (golem *output-file* :direction :output :if-exists :supersede)
		   (print-pattern-list *patterns* golem :support *method*))
	       (file-error (cond)
		 (error 'simple-error
			:format-control "cannot write to file: ~a, ~a"
			:format-arguments (list *output-file* cond))))) 
    (error (cond)
      (format t "An error occured during processing: ~a~%" cond)
      (dendron-quit))))
