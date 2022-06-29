#||
CSE 391  Assignment #2
Derron Simon (205-56-0093)

Code for general-search, breadth-first, depth-first-depth-limit,
breadth-first-no-duplicates and depth-first-depth-limit-no-duplicates.

I did not implement best-first because I had a very hard time with
implementing the get-cost.  I kept falling over the lisp syntax.  What
I would have done is give each node a value based on the total distance
of numbers from their goal position.  I then would have to add it to the
*closed* list and sort the list by cost.  I didn't have the time to
understand the array stuff, though I probably could (I didn't understand
LISP until 2 weeks ago!).

Testing:
 All this code has been tested and works.  There is no mention of any
 test we need to turn in, so I leave you just the code.

 I tested with: (pppp (general-search (randomize-puzzle (make-puzzle)) #'alg))

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
#||
File:		8-puzzle.lisp
Description:	Support functions for the students for the 8-puzzle search
		problem.
||#
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
(in-package "USER")
;---------------------------------------------------------------------------

; some global variables . . .

(defvar *goal*)			; the goal configuration


(defvar *open*)			; a list called *open*
(defvar *closed*)		; a list called *closed*

(defvar *depth-limit*	20)	; search tree depth limit

(defvar *node-count*)		; number of nodes generated during search
;---------------------------------------------------------------------------

; the 8-puzzle data structure . . .

(defstruct (puzzle
	      (:print-function
		 (lambda (struct stream depth)
			 (declare (ignore depth))
			 (format stream "~% -----------")
			 (format stream "~%| ~A | ~A | ~A |"
				 (aref (puzzle-board struct) 0 0)
				 (aref (puzzle-board struct) 0 1)
				 (aref (puzzle-board struct) 0 2))
			 (format stream "    blank is at row=~D, col=~D"
				 (puzzle-blank-row struct)
				 (puzzle-blank-col struct))
			 (format stream "~%| ~A | ~A | ~A |"
				 (aref (puzzle-board struct) 1 0)
				 (aref (puzzle-board struct) 1 1)
				 (aref (puzzle-board struct) 1 2))
			 (format stream "    cost  = ~D" (puzzle-cost struct))
			 (format stream "~%| ~A | ~A | ~A |"
				 (aref (puzzle-board struct) 2 0)
				 (aref (puzzle-board struct) 2 1)
				 (aref (puzzle-board struct) 2 2))
			 (format stream "    depth = ~D" (puzzle-depth struct))
			 (format stream "~% -----------~%"))))
  (board (make-array '(3 3) :initial-contents '((1 2 3) (4 7 5) (6 X 8))))
  (blank-row 2)		; location of blank on the board
  (blank-col 1)
  parent		; parent node
  (depth 0)		; current depth of this node
  (cost 0)		; cost assigned to this node
  )
;---------------------------------------------------------------------------
(defun pppp (puzz)
  "pppp = pretty print puzzle path in reverse i.e. start to goal"
  (when (not (null puzz))
	(pppp (puzzle-parent puzz))
	(print puzz))
  nil)
;---------------------------------------------------------------------------
(defun duplicate-board (b)
  "this function returns a duplicate of the board, b"
  (let ((new-b (make-array '(3 3))))
       (setf (aref new-b 0 0) (aref b 0 0))
       (setf (aref new-b 0 1) (aref b 0 1))
       (setf (aref new-b 0 2) (aref b 0 2))
       (setf (aref new-b 1 0) (aref b 1 0))
       (setf (aref new-b 1 1) (aref b 1 1))
       (setf (aref new-b 1 2) (aref b 1 2))
       (setf (aref new-b 2 0) (aref b 2 0))
       (setf (aref new-b 2 1) (aref b 2 1))
       (setf (aref new-b 2 2) (aref b 2 2))
       new-b))

(defun duplicate-puzzle (p)
  "this function returns a duplicate of the puzzle, p with p as its parent"
  (incf *node-count*)
  (make-puzzle :board (duplicate-board (puzzle-board p))
	       :blank-row (puzzle-blank-row p)
	       :blank-col (puzzle-blank-col p)
	       :parent p
	       :depth (1+ (puzzle-depth p))))
;---------------------------------------------------------------------------
(defun move-up? (p)
  "see if we can move the blank in the puzzle up"
  (if (<= (puzzle-blank-row p) 0)
      nil
      t))

(defun move-up (p)
  "move the blank in the puzzle up (if possible)"
  (assert (> (puzzle-blank-row p) 0))
  (let ((tmp (aref (puzzle-board p)
		   (1- (puzzle-blank-row p)) (puzzle-blank-col p))))
       (setf (aref (puzzle-board p)
		   (1- (puzzle-blank-row p)) (puzzle-blank-col p))
	     'X)
       (setf (aref (puzzle-board p)
		   (puzzle-blank-row p) (puzzle-blank-col p))
	     tmp)
       (decf (puzzle-blank-row p))
       )
  p)
;---------------------------------------------------------------------------
(defun move-down? (p)
  "see if we can move the blank in the puzzle down"
  (if (>= (puzzle-blank-row p) 2)
      nil
      t))

(defun move-down (p)
  "move the blank in the puzzle down (if possible)"
  (assert (< (puzzle-blank-row p) 2))
  (let ((tmp (aref (puzzle-board p)
		   (1+ (puzzle-blank-row p)) (puzzle-blank-col p))))
       (setf (aref (puzzle-board p)
		   (1+ (puzzle-blank-row p)) (puzzle-blank-col p))
	     'X)
       (setf (aref (puzzle-board p)
		   (puzzle-blank-row p) (puzzle-blank-col p))
	     tmp)
       (incf (puzzle-blank-row p))
       )
  p)
;---------------------------------------------------------------------------
(defun move-left? (p)
  "see if we can move the blank in the puzzle to the left"
  (if (<= (puzzle-blank-col p) 0)
      nil
      t))

(defun move-left (p)
  "move the blank in the puzzle to the left (if possible)"
  (assert (> (puzzle-blank-col p) 0))
  (let ((tmp (aref (puzzle-board p)
		   (puzzle-blank-row p)
		   (1- (puzzle-blank-col p)))))
       (setf (aref (puzzle-board p)
		   (puzzle-blank-row p)
		   (1- (puzzle-blank-col p)))
	     'X)
       (setf (aref (puzzle-board p)
		   (puzzle-blank-row p)
		   (puzzle-blank-col p))
	     tmp)
       (decf (puzzle-blank-col p))
       p))
;---------------------------------------------------------------------------
(defun move-right? (p)
  "see if we can move the blank in the puzzle to the right"
  (if (>= (puzzle-blank-col p) 2)
      nil
      t))

(defun move-right (p)
  "move the blank in the puzzle to the right (if possible)"
  (assert (< (puzzle-blank-col p) 2))
  (let ((tmp (aref (puzzle-board p)
		   (puzzle-blank-row p)
		   (1+ (puzzle-blank-col p)))))
       (setf (aref (puzzle-board p)
		   (puzzle-blank-row p)
		   (1+ (puzzle-blank-col p)))
	     'X)
       (setf (aref (puzzle-board p)
		   (puzzle-blank-row p)
		   (puzzle-blank-col p))
	     tmp)
       (incf (puzzle-blank-col p))
       p))
;---------------------------------------------------------------------------
(defun randomize-puzzle (p)
  "randomize the contents of a puzzle"
  (dotimes (i 10)
	   (ecase (random 4)
		  (0	(if (move-left? p)	(move-left p)))
		  (1	(if (move-up? p)	(move-up p)))
		  (2	(if (move-right? p)	(move-right p)))
		  (3	(if (move-down? p)	(move-down p)))))
  p)

;--------------------------------------------------------------
(defun breadth-first (node)
  (setq *open* (append *open* node))
  )

(defun depth-first (node)
  (setq *open* (append node *open*))
  )

(defun depth-first-depth-limit (node)
  (if (< (puzzle-depth (first node)) *depth-limit*)
	(setq *open* (append node *open*)))
  )

(defun compare-nodes (node1 node2)
  "used by no-duplicates functions to test for duplicates"
  (equalp (puzzle-board node1) (puzzle-board node2))
  )

(defun breadth-first-no-duplicates (node)
  (if (find (first node) *closed* :test #'compare-nodes)
      'nil
      (setq *open* (append *open* node)))
  )

(defun depth-first-depth-limit-no-duplicates (node)
  (if (< (puzzle-depth (first node)) *depth-limit*)
      (if (find (first node) *closed* :test #'compare-nodes)
	  'nil
	(setq *open* (append node *open*))))
  )

(defun get-cost (node)
  )

(defun best-first (node)
  )

(defun general-search (start q-strategy)
  (setf *open* nil)
  (push start *open*)
  (setf *closed* nil)
  (defvar node)
  (setf *node-count* 0)

  (loop while (consp *open*) do
	(setf node (pop *open*))
	(push node *closed*)
	(when (equalp (puzzle-board node) (puzzle-board *goal*)) (return node))
	(when (move-up? node)
	      (funcall q-strategy (list (move-up (duplicate-puzzle node)))))
	(when (move-down? node)
	      (funcall q-strategy (list (move-down (duplicate-puzzle node)))))
	(when (move-left? node)
	      (funcall q-strategy (list (move-left (duplicate-puzzle node)))))
	(when (move-right? node)
	      (funcall q-strategy (list (move-right (duplicate-puzzle node)))))
 	)
  )

(setf *goal* (make-puzzle :board (make-array '(3 3)
				:initial-contents
				'((1 2 3)
				  (4 X 5)
				  (6 7 8)))
				:blank-row 1
				:blank-col 1))
;---------------------------------------------------------------------------

