;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-

(in-package "USER")

(proclaim '(optimize (safety 3) (speed 0)))

(defun length>1 (list)
  "Is this a list of 2 or more elements?"
  (and (consp list) (cdr list)))

(defun average (numbers)
  "Numerical average (mean) of a list of numbers."
  (/ (sum numbers) (length numbers)))

(defun sum (numbers &optional (key #'identity))
  "Add up all the numbers; if KEY is given, apply it to each number first."
  (if (null numbers)
      0
      (+ (funcall key (first numbers)) (sum (rest numbers) key))))

(defun required ()
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (error "A required argument was not supplied."))

(defun random-integer (from to)
  "Return an integer chosen at random from the given interval."
  (+ from (random (+ 1 (- to from)))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun mappend (fn &rest lists)
  "Apply fn to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun fuzz (quantity &optional (proportion .1) (round-off .01))
  "Add and also subtract a random fuzz-factor to a quantity."
  (round-off (+ quantity
		(* quantity (- (random proportion) (random proportion))))
	     round-off))

(defun round-off (number precision)
  (* precision (round number precision)))

(defun constantly (value)
  "Return the function that always returns VALUE."
  #'(lambda (&rest args) (declare (ignore args)) value))

(defmacro with-function-counts (fns &rest body)
  `(unwind-protect
    (progn ,@(mapcar #'(lambda (fn)
		       `(progn
			 (defadvice (,fn function-count) (&rest args)
			   (incf (get ',fn 'function-count))
			   (apply-advice-continue args))
			 (setf (get ',fn 'function-count) 0)))
		   fns)
	   ,@body
	   (mapcar #'(lambda (fn)
		       (list fn (get fn 'function-count)))
            ',fns))
    (mapcar #'(lambda (fn)
		(remove-advice fn 'function-count :verbose nil))
            ',fns)))

(defmacro do-for ((var start end &key (by 1)) &body body)
  ;;(with-gensyms (start-var end-var by-var)
  `(let ((end-var ,end)
	 (by-var ,by))
    (do ((,var ,start (+ ,var by-var)))
	((if (> by-var 0) (> ,var end-var) (< ,var end-var)))
      ,@body)))

(defun map-times (fn times)
  (if (<= times 0)
      nil
      (cons (funcall fn times) (map-times fn (- times 1)))))

;;; An expression is a list consisting of a prefix operator followed by args,
;;; Or it can be a symbol, denoting an operator with no arguments.

(defun make-exp (op &rest args) (cons op args))
(defun op (exp) (if (listp exp) (first exp) exp))
(defun args (exp) (if (listp exp) (rest exp) nil))

(defun map-file (fn file &key (read #'read))
  "Apply fn to every expression in file."
  (with-open-file (stream file :direction :input)
    (let ((eof "EOF"))
      (loop for x = (funcall read stream nil eof) until (eq x eof)
	    do (funcall fn x)))))

(defmacro do-file ((var file &key (by '#'read)) &body body)
  `(map-file #'(lambda (,var) ,@body) ,file :read ,by))

(defun starts-with (list element)
  "Is this a list that starts with the given element?"
  (and (consp list) (eq (first list) element)))

(defun nothing (&rest args)
  (declare (ignore args))
  nil)

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun stringify (exp)
  (cond ((stringp exp) exp)
	((symbolp exp) (symbol-name exp))
	(t (format nil "~A" exp))))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))

(defun last1 (list)
  (first (last list)))

(defun transpose (list-of-lists)
  "Transpose a matrix represented as a list of lists.
  Example: (transpose '((a b c) (d e f))) => (a d) (b e) (c f)."
  (apply #'mapcar #'list list-of-lists))

(defmacro o (x)
  `(progn (format *debug-io* "~&~S = "  ',x) (prin1 ,x *debug-io*)))

;;;; Examples Facility

(defvar *examples* nil)

(defstruct (example (:print-function print-example))
  name system examples)

(defmacro defexamples (name (&key system) &rest examples)
  `(first (setf *examples*
	   (cons
	    (make-example :name ',name :system ',system :examples ',examples)
	    (delete ',name *examples* :key #'example-name)))))

(defun run-examples (name)
  "Run the examples with the given name.  NAME can also be :all to run
  everything, or it can be an examples structure."
  (let ((*print-pretty* t))
    (cond ((eq name :all) (mapc #'run-examples *examples*))
	  ((example-p name)
	   (dolist (exp (example-examples name))
	     (if (stringp exp)
		 (format t "~2&;; ~A~%" exp)
		 (progn
		   (format t "~2&> ") (write exp)
		   (format t "~&") (write (eval exp))))))
	  ((find name *examples* :key #'example-name)
	   (run-examples (find name *examples* :key #'example-name)))
	  (t (warn "Example named ~A not found." name))))
  (values))

(defun print-example (example &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "[~A~@[ (system ~A)~]: ~D example~:p]"
	  (example-name example) (example-system example)
	  (length (example-examples example))))
  
;; Additional function needed for running environments, sent separately
;; by Peter Norvig on 15 September 1993

(defun arg1 (exp) (first (args exp)))


;; Maybe a systems facility?
;; Define a directory for all the code.
;; Load into that directory. (Compile also).
;; Have it prompt for the directory.
