;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-

(in-package "USER")

;;;; The Vacuum World

(defstruct (dirt (:include object (name "*") (size .01))))

(defun vacuum-trials (&key (n 100) (agent-types '(reactive-vacuum-agent
						  vacuum-agent-with-state
						  systematic-vacuum-agent)))
  "Run the vacuum world for different agents; print average scores."
  (agent-trials #'vacuum-world agent-types :n n))

(defun vacuum-world (&key (dirt .25) (x-size 8) (y-size x-size)
			  (agents (list (systematic-vacuum-agent 'A))))
  (initialize-world :agents agents :walled t
		    :objects `((dirt ,dirt))
		    :x-size x-size :y-size y-size
		    :score-fn #'vacuum-performance-fn
		    :percept-fn #'vacuum-world-percept))

;;;> (vacuum-trials)
;;;     97.82 average for REACTIVE-VACUUM-AGENT
;;;    318.06 average for VACUUM-AGENT-WITH-STATE
;;;    799.31 average for SYSTEMATIC-VACUUM-AGENT

;;::  Running a vacuum agent step by step

(defun run-vacuum-agent (&key (agent (systematic-vacuum-agent 'A))
			      (max-time 10) (x-size 8))
  "Run an agent in a vacuum world."
  (run-world (vacuum-world :agents (list agent) :x-size x-size)
	     :max-time max-time ))

;;;; Percepts

(defun vacuum-world-percept (agent world)
  "Percept vector is a three-element sequence: bump, dirt and home."
  (let ((loc (agent-loc agent)))
    (list (if (agent-bump agent) 'bump)
	  (if (find-object-if #'dirt-p loc world) 'dirt)
	  (if (equal loc (world-start world)) 'home))))

;;;; Performance Function

(defun vacuum-performance-fn (agent world)
  "To be precise, the performance measure will be 100
  points for each piece of dirt vacuumed up, -1 point for each action
  taken, and -1000 points if it is not in the home location when it
  turns itself off or runs out of time."
  (+ (* 100 (count-if #'dirt-p (agent-carrying agent)))
     (- (world-time world))
     (if (and (not (agent-alive-p agent))
	      (not (equal (agent-loc agent) (make-loc :x 1 :y 1))))
	 -1000
	 0)))

;;;; Actions

;;; The actions FORWARD and TURN are joined by two more:

(defun suck (agent world)
  (let* ((loc (agent-loc agent))
	 (dirt (find-object-if #'dirt-p loc world)))
    (when dirt
      (push dirt (agent-carrying agent))
      (remove-object dirt world))))

(defun shut-off (agent world)
  (declare (ignore world))
  (setf (agent-alive-p agent) nil))

;;;; Agents

(defun reactive-vacuum-agent (name)
    (make-agent
     :name name
     :program
     #'(lambda (percept)
	 (let ((bump (first percept))
	       (dirt (second percept))
	       (home (third percept)))
	   (cond (dirt 'suck)
		 (home (random-element '(shut-off forward (turn right))))
		 (bump (random-element '((turn right) (turn left))))
		 (t (random-element '(forward forward forward
				    (turn right) (turn left)))))))))

(defun vacuum-agent-with-state (name)
  (make-agent
   :name name
   :program
   (let ((time-since-last-pickup 0))
     #'(lambda (percept)
	 (incf time-since-last-pickup)
	 (let ((bump (first percept))
	       (dirt (second percept))
	       (home (third percept)))
	   (cond (dirt (setf time-since-last-pickup 0) 'suck)
		 ((and home (> time-since-last-pickup 10))
		  'shut-off)
		 (bump (random-element '((turn right) (turn left))))
		 (t (random-element '(forward forward forward
				      (turn right) (turn left))))))))))

(defun systematic-vacuum-agent (name)
  (make-agent
   :name name
   :program
   (let ((orientation 0)
	 (plan nil)
	 (done nil))
     #'(lambda (percept)
	 (let ((bump (first percept))
	       (dirt (second percept))
	       (home (third percept)))
	   (let ((action
		  (cond
		    (dirt 'suck)
		    ((and home done) 'shut-off)
		    (bump (case orientation
			    (0 (setq plan '((turn left) forward (turn left))))
			    (90 (setq done t)
				(setq plan '((turn left) (turn left))))
			    (180 (setq plan '((turn right) forward
					      (turn right))))
			    (270 (setq plan '((turn right)))))
			  (pop plan))
		    (plan (pop plan))
		    (t 'forward))))
	     (when (eq (op action) 'turn)
	       (setq orientation
		     (add-degrees orientation
				  (direction->degrees (arg1 action)))))
	     action))))))
