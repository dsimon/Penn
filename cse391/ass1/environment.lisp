;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-

(in-package "USER")

#||
;;;; Environment (or World) Simulator

This file implements the environment simulator.  An environment contains a
list of agents that move about in locations, possibly encountering objects.
In this implementation we use the word "world" instead of "environment"
because it is shorter.  We would like the simulator to work for lots of
different worlds and agents, so there are very few assumptions built in.
Instead, each world contains several functions that distinguish the
world from other world, and determine what the agents can do and
what they perceive.  At each time step the simulator does the following:

  Call the done-fn to see if the simulation is over.
  Call the percept-fn to give each agent its percepts
  Call each agent's program to get an action for each agent
  Call the change-fn to execute the actions and possibly make
       spontaneous changes to the world.

At the end of the simulation the score-fn can be called to determine each
agent's performance.  The default change-fn is EXECUTE-ACTIONS, which executes
each action in turn.  If you want, you can supply a function to handle
simultaneous action in a better way.  EXECUTE-ACTIONS also calls the
world's interaction function so that each agent interacts with each
object in its current location.  If you want agents to interact with objects
farther away, you'll need to supply a different change-fn or interaction-fn.

A few assumptions are made.  The world consists of a 2-dimensional grid
of locations.  (If you don't like that, you can change the code, or you can
make the world be a 1x1 grid, and do anything you want with the one
location.)  The locations are all the same size, loc-size.  Each object has a
size, and The total size of the objects in a location cannot exceed loc-size.
The function MOVE-OBJECT is provided to move an object from one location to
another.  The functions FORWARD and TURN are intended as actions for agents;
FORWARD moves an agent one step straight ahead and TURN changes the agent's
orientation.  

||#

;;;; Data Structures for Worlds, Objects and Agents

(defstruct (world (:print-function print-world))
  "A world (or environment) is everything that is."
  (time 0 :type number)			; Number of steps taken so far
  (grid (make-array '(10 10)))		; A 2-D array of squares
  (loc-size 10 :type number)		; Size of a square in meters
  (start '(1 1) :type location)		; Where agents begin
  (agents '() :type list)		; List of agents in the world
  (objects '() :type list)		; List of objects in the world
  (sounds '() :type list)		; Alist of (object . sound) pairs
  (done-fn #'false :type (function (world) t))
  (percept-fn #'false :type (function (agent world) action))
  (change-fn #'execute-actions :type (function (list world) t))
  (interaction-fn #'nothing :type (function (object object world) t))
  (score-fn (constantly 0) :type (function (agent world) number)))

(defstruct (loc (:type list)) x y)	; Specifies a square in x,y coordinates

(deftype action () '(or symbol list))	; E.g. FORWARD or (TURN RIGHT)

(deftype percept () 't)			; A percept can be anything

(defstruct object
  "An object is anything that occupies space.  Some objects are alive, and some
  are structured objects that contain sub-objects nested in a sub-grid."
  (name '?)				; Used to print the object on the map
  (world)				; The world that the object is in
  (loc)					; The square that the object is in
  (size 1)				; Rough diameter of object in meters
  (alive-p nil)				; Is the object now living?
  (bump nil)				; Has the object bumped into something?
  (color 'black)			; Some agents can see object's color
  (shape 'rectangle)			; Some agents can see object's shape
  (features nil)			; Misc. list of features
  (nested-world nil))			; Structured objects have a nested world

(defstruct (agent (:include object (alive-p t)
			    (size .8) (shape 'cylinder) (color 'metallic))
		  (:print-function print-agent))
  "An agent is any entity that can take action."
  (program (required) :type (function (percept) action))
  (holding nil)				; Some have a hand to hold 1 thing
  (holding-max-size .5)			; Biggest thing that can be held
  (carrying '())			; Some agents can carry things
  (orientation 0)			; The direction the agent is facing
  (action nil :type action)		; The action the agent has decided on
  (entered-stack nil)			; Nested worlds the agent has entered
  (zoomed-at nil))			; Where the camera (if any) is focused

(defstruct (wall (:include object (name "#") (size 10) (shape 'slab))))

(defvar *trace-world* t
  "If true, information on the world will be printed on each step.")

;;;; Running Agents in the World

(defun run-world (world &key (max-time 10))
  "Run the world one step at a time until done.  Return the world.
   15 Sept: max-time changed to 10, for cse391 tracing.
   15 Sept: yes-or-no-p added, for cse391 tracing."
  (let ((agents (world-agents world)))
    (loop
     (with-simple-restart
	 (abort "Abort to RUN-WORLD top level loop")
       (when (end-of-game-p agents world max-time)
	 (RETURN))
       (when *trace-world*
	 (display-world world))
       (incf (world-time world))
       ;; Present each agent with a percept, and have it choose an action
       (dolist (agent agents)
	 (setf (agent-action agent)
	       (determine-action agent (compute-percept agent world))))
       ;; Carry out the actions
       (cond (*trace-world* (yes-or-no-p 
			     "The time is ~A. There are ~A ticks left. Act now?"
                              (world-time world)
                              (- max-time (world-time world)))))
       (funcall (world-change-fn world) agents world))))
  world)

(defun end-of-game-p (agents world max-time)
  "The game ends after the given number of turns, or when
  the agents are all dead or when the world's DONE function is satisfied."
  (or (>= (world-time world) max-time)
      (not (some #'agent-alive-p agents))
      (funcall (world-done-fn world) world)))

(defun compute-percept (agent world)
  "Determine the percept that this agent gets from the world."
  (funcall (world-percept-fn world) agent world))

(defun determine-action (agent percept)
  "Present the agent with its percept and determine its action."
  (when (agent-alive-p agent)
    (let ((action (funcall (agent-program agent) percept)))
      (when *trace-world*
	(format t "~&~A does ~A" agent  action))
      action)))

(defun execute-actions (agents world)
  "The default change function: execute the actions one at a time.
  Also make sure that the bump wears off and sounds dissipate."
  (setf (world-sounds world) nil)
  (dolist (agent agents)
    (setf (agent-bump agent) nil))
  (dolist (agent agents)
    (execute-action agent (agent-action agent) world)))

(defun execute-action (agent action world)
  "Execute the action by applying the function to its arguments.
  Then call the interaction function for each object in the agent's location."
  ;; More sophisticated functions would limit the actions that agents can do
  (unless (null action)
    (apply (op action) agent world (args action)))
  ;; The agent interacts with every object in the location except itself.
  (dolist (object (world-contents (agent-loc agent) world))
    (unless (eq object agent)
      (funcall (world-interaction-fn world) agent object world))))

(defun human-agent (&optional (name 'H) (constructor 'make-agent))
  "Return an agent that does whatever a human user says it should do."
  (funcall constructor
	   :name name
	   :program #'(lambda (percept)
			(format t "~&~A perceives ~A and does: " name percept)
			(read))))

;;;; Scoring of Agents in Worlds

(defun agent-trials (world-fn agent-types &key (n 10) (max-time 15))
  "Report how well agents do in a series of N similar worlds.
  World-fn takes a :agents keyword argument, and returns an world.
  agent-types are names of functions that each create an agent.
  15 Sept: max-time changed from 1000 to 15 for vacuum-world."
  (dolist (agent-type agent-types)
    (format t "~&~10,2F average for ~A"
	    (first
	     (score-worlds
	      #'(lambda ()
		  (funcall world-fn
			   :agents (list (funcall agent-type agent-type))))
	      :n n :max-time max-time))
	    agent-type)))

(defun score-worlds (world-fn &key (n 10) (max-time 15))
  "Return the average score for each agent over N different worlds,
  each created by world-fn, a function of no arguments that returns
  an world (it should have some random component).
  15 Sept: max-time changed from 1000 to 15 for vacuum-world."
  (let (  ;; (*trace-world* nil)
	(worlds nil))
    (dotimes (i n)
      (push (funcall world-fn) worlds))
    (average-world-scores
     (mapcar #'(lambda (world) (run-world world :max-time max-time))
	     worlds))))

(defun average-world-scores (worlds)
  "The average scores for each agent over the worlds."
  (mapcar #'average (transpose (mapcar #'world-scores worlds))))

(defun world-scores (world)
  "A list of the scores for the agents in the world."
  (mapcar #'(lambda (agent)
	      (funcall (world-score-fn world) agent world))
	  (world-agents world)))

;;;; Printing

(defun display-world (world)
  "Print a map of any worlds or nested world with agents in them."
  (mapc #'print-world-map
	(delete-duplicates (mapcar #'agent-world (world-agents world)))))

(defun print-world-map (world)  
  ;; Print the header
  (format t "~&Agents ~{~A~^, ~}:" (world-agents world))
  (format t "~&   ")
  (dotimes (x (world-x-size world))
    (format t "|---"))
  (format t "|~%")
  ;; Print each row
  (do-for (y (- (world-y-size world) 1) 0 :by -1)
    (format t "~2D " y)
    ;; Print each location
    (dotimes (x (world-x-size world))
      (format t "|~3A" (loc-string x y world)))
    (format t "|~%   ")
    (dotimes (x (world-x-size world))
      (format t "|---"))
    (format t "|~%"))
  ;; Print the X-coordinates along the bottom
  (format t "   ")
  (dotimes (x (world-x-size world))
    (format t " ~2D " x))
  world)

(defun print-world (world &optional (stream t) depth)
  (declare (ignore depth world))
  (format stream "<World with ~D objects>" (length (world-objects world))))

(defun print-agent (agent &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "[~A at ~A:~D~@[ holding ~A~]]" (agent-name agent)
	  (agent-loc agent) (agent-orientation agent)
	  (agent-holding agent)))

(defun world-x-size (world)
  (array-dimension (world-grid world) 0))

(defun world-y-size (world)
  (array-dimension (world-grid world) 1))

(defun loc-string (x y world)
  "Return a string of 3 characters describing what is in this location."
  (let ((names (mapcar #'object-name (aref (world-grid world) x y))))
    (subseq (format nil "~{~A~}   " names) 0 3)))


;;;; Initializing Worlds

(defun initialize-world (&rest args
			       &key (x-size 10) (y-size x-size) objects agents
			       world (start '(1 1)) (walled nil)
			       &allow-other-keys)
  "Build a new world with all the agents at start,
  and various objects scattered in random locations."
  (when (null world)
    (setf world (apply #'make-world :allow-other-keys t
		       :grid (make-array (list x-size y-size)
					 :initial-element '())
		       args)))
  ;; Add agents, maybe walls, and other objects
  (dolist (agent agents)
    (place-object agent start world))
  (when walled
    (wall-up world))
  (dolist (object-specification objects)
    (initialize-objects object-specification world))
  world)

(defun wall-up (world)
  "Put walls around the edge of the world."
  (let ((x-size (world-x-size world))
	(y-size (world-y-size world))
	(size (world-loc-size world)))
    (dotimes (i x-size)
      (place-object (make-wall :size size) (make-loc :x i :y 0) world)
      (place-object (make-wall :size size) (make-loc :x i :y (- y-size 1))
		    world))
    (dotimes (i (- y-size 1))
      (when (> i 0)
	(place-object (make-wall :size size) (make-loc :x 0 :y i) world)
	(place-object (make-wall :size size) (make-loc :x (- x-size 1) :y i)
		      world)))
    world))
  
(defun initialize-objects (object-specification world)
  "Place objects in the world.  An object specification is a list of the
  form: (type number locations sub-objects . features).
  The number is either an integer, saying how many objects to place, or a
  fraction giving the probability of an object in any square.  The locations
  can be either a single location: (2 2), or a list: ((2 2) (2 4)), or nil to
  stand for the whole world.  Examples:
    (pit 1 (2 2))        - Place a pit in location 2 2.
    (pit 10)             - Place 10 pits, scattered at random.
    (pit 1 ((2 2) (2 4)) - Place 1 pit in location 2 2 and one in 2 4.
    (pit .01 (1 1))      - The start square has a 1% chance of having a pit.
    (pit .25)            - Each square has a 25% chance of having a pit in it.
    (display-case 1 (2 2) (tomatoe 10) ((sign Tomatoes $ .89 lb) 1))
                         - Square 2 2 has a display-case containing 10 tomatoes
                           and a sign with the given words.
  No object is placed if there is no room for it, and nothing is placed
  in the start square unless you explicitly specify that square."
  (let* ((type (first object-specification))
	 (num (second object-specification))
	 (loc (third object-specification))
	 (locs (if (integerp (first loc))
		   (list loc)
		   loc))
	 (other-args (nthcdr 3 object-specification)))
    (cond ((and (integerp num) locs)	; Place NUM objects in LOC
	   (dolist (loc locs)
	     (assert (loc-p loc))
	     (dotimes (i num)
	       (place-object (make-individual type other-args) loc world))))
	  ((integerp num)		; Place NUM objects randomly
	   (dotimes (i num)
	     (place-object-randomly (make-individual type other-args) world)))
	  (locs				; Place 1 object in LOC with Prob=NUM
	   (dolist (loc locs)
	     (place-with-probability type loc world num other-args)))
	  (t				; Each square has Prob=NUM of an object
	   (dotimes (x (world-x-size world))
	     (dotimes (y (world-y-size world))
	       (let ((loc (make-loc :x x :y y)))
		 (unless (equal loc (world-start world))
		   (place-with-probability type loc world num
					   other-args)))))))))

(defun place-with-probability (type loc world probability other-args)
  "Put an object of the given type in loc with the given probability."
  (when (and (< (random 1.0) probability))
    (place-object (make-individual type other-args) loc world)))

(defun place-object-randomly (object world)
  "Place object somewhere where there is room for it, but not at start."
;; 29 Sept 1993 - 10 changed to 1000 in DOTIMES, to accommodate small
;;                worlds where it may take longer to find a place to put
;;                an object.
;; 29 Sept 1993 - Commented out restriction that object not placed at
;;                start. Overly restrictive for small worlds.
  (dotimes (i 1000 (cerror "Forget placing ~A in world."
			 "No room for ~A in world ~A." object world))
    (let ((loc (random-loc world)))
      (when (and 
;;               (not (equal loc (world-start world)))
		 (place-object object loc world))
	(RETURN object)))))

(defun random-loc (world)
  "Return a random loc, somewhere in the world."
  (make-loc :x (random (world-x-size world))
	    :y (random (world-y-size world))))

(defun make-individual (type other-args)
  (let* ((sub-objects (first other-args))
	 (init-args (rest other-args))
	 (individual (apply (concat-symbol 'make- type)
			    :allow-other-keys t init-args)))
    (when sub-objects
      (setf (object-nested-world individual)
	    (apply #'initialize-world :objects sub-objects
		   :world (object-nested-world individual)
		   init-args)))
    individual))

(defun place-object (object loc world)
  "Put the object in its initial position in the world."
  (assert (loc-p loc))
  (when (room-for object loc world)
    (setf (object-loc object) loc)
    (setf (object-world object) world)
    (pushnew object (world-objects world))
    (pushnew object (aref (world-grid world) (loc-x loc) (loc-y loc)))
    (when (agent-p object)
      (pushnew object (world-agents world)))
    object))

;;;; Dealing with Objects

(defun move-object (object loc world)
  "Try to move object to a new location, and return that location. However,
  an object can't move into a location that doesn't have room.  If there is
  no room, the object gets a bump, and nil is returned."
  (assert (loc-p loc))
  (if (room-for object loc world)
      (progn
	(when (object-loc object)
	  (remove-object object world))
	(place-object object loc world)
	loc)
      (progn
	(setf (object-bump object) 'bump)
	;;(warn "~A tried to move to ~A, which has ~A and is size ~D" object loc
	;;(world-contents loc world) (world-loc-size world))
	nil)))

(defun remove-object (object world)
  "Remove the object from its current location, and from its world."
  (let ((loc (object-loc object)))
    (setf (object-world object) nil)
    (setf (world-objects world) (delete object (world-objects world)))
    (setf (aref (world-grid world) (loc-x loc) (loc-y loc))
	  (delete object (world-contents loc world)))
    (when (agent-p object)
      (setf (world-agents world)
	    (delete object (world-agents world))))))

(defun world-contents (loc world)
  "Return a list of objects in this location."
  (aref (world-grid world) (loc-x loc) (loc-y loc)))

(defun find-object-if (predicate loc world)
  "Return an object in this location that satisfies this predicate."
  (find-if predicate (world-contents loc world)))

(defun find-neighbor-if (predicate loc world)
  "Return an object in a neighboring square that satisfies the predicate."
  (let ((x (loc-x loc))
	(y (loc-y loc)))
    ;; Look in the four neighboring squares
    (or (find-object-if predicate (make-loc :x x :y (+ y 1)) world)
	(find-object-if predicate (make-loc :x x :y (- y 1)) world)
	(find-object-if predicate (make-loc :x (+ x 1) :y y) world)
	(find-object-if predicate (make-loc :x (- x 1) :y y) world))))

(defun find-object-or-neighbor-if (predicate loc world)
  "Return an object either in loc or a neighboring square that satisfies
  the predicate."
  (or (find-object-if predicate loc world)
      (find-neighbor-if predicate loc world)))

(defun room-for (object loc world)
  "Is there room for the object in the given location? There is unless
  the sum of the sizes of all objects would exceed the location's size."
  (<= (+ (object-size object)
	 (sum (world-contents loc world) #'object-size))
      (world-loc-size world)))

;;;; Structured Objects, Nested Environments, and Fractional Locations

(defun loc-and-world (loc world)
  "Return the location and sub-world pointed at by a possibly fractional loc.
  If both coordinates are whole numbers, this is just the loc and world.
  Otherwise, if there is a structured object in the world, it is a location
  within the structured object."
  (let ((object (find-object-if #'structured-object-p (outer-loc loc) world)))
    (cond ((and object (not (every #'integerp loc)))
	   (list (nested-loc loc) (object-nested-world object)))
	  (t (list loc world)))))

(defun nested-move-object (object new-loc outer-world)
  (apply #'move-object object (loc-and-world new-loc outer-world)))

(defun nested-world-contents (loc outer-world)
  (apply #'world-contents (loc-and-world loc outer-world)))

(defun outer-loc (loc)
  "Return the loc in the outer world, i.e. ignore the fractional part."
  (mapcar #'floor loc))

(defun nested-loc (loc)
  "Return the loc within the nested world. Eg: (nested-loc '(1.2 1.3)) = (2 3)"
  (mapcar #'(lambda (n) (* 10 (rem (rationalize n) 1))) loc))

(defun whole-number-p (n)
  (integerp (rationalize n)))

(defun loc-p (x)
  "Is this a specification of a location?"
  (and (listp x) (= (length x) 2) (every #'numberp x)))

(defun structured-object-p (x)
  (and (object-p x) (object-nested-world x)))

;;;; Maintianing Orientation

(defun add-degrees (degree1 degree2)
  "Add two quantitites in degrees such that 0 <= result < 360"
  (mod (+ degree1 degree2) 360))

(defun add-locs (&rest locations)
  "Coordinate-wise addition of locations: (add-locs '(1 2) '(10 20)) = (11 22)"
  (apply #'mapcar #'+ locations))

(defun subtract-locs (&rest locations)
  "Coordinate-wise subtraction of locations: (add-locs  '(9 8) '(1 1)) = (8 7)"
  (apply #'mapcar #'- locations))

(defun direction->degrees (direction)
  "Translate a direction like LEFT or RIGHT into degrees like +90 or -90"
  (case direction
    (left    +90)
    (right   -90)
    (around +180)
    (otherwise 0)))

(defun orientation->offset (orientation)
  "Convert an orientation like 90 to an offset like (0 1)."
  (case orientation
    (  0 '(1 0))
    ( 90 '(0 1))
    (180 '(-1 0))
    (270 '(0 -1))))

(defun location-towards (loc orientation)
  "One square from loc in the given direction."
  (add-locs loc (orientation->offset orientation)))

(defun absolute-loc (agent offset)
  "Return an absolute location given an offset from an agent, taking the agent's
  orientation into account.  However, if the offset rounds to (0 0), then it
  is within the agent itself, and since the agent rotates, we don't need to
  adjust for orientation."
  (let ((x (loc-x offset))
	(y (loc-y offset)))
    (if (equal (outer-loc offset) '(0 0))
	(add-locs (agent-loc agent) offset)
	(add-locs (agent-loc agent)
		  (case (agent-orientation agent)
		    (  0 (make-loc :x y :y (- x)))
		    ( 90 offset)
		    (180 (make-loc :x (- y) :y x))
		    (270 (make-loc :x (- x) :y (- y))))))))

(defun offset-loc (agent loc)
  "Return an offset from an agent that corresponds to the absolute loc."
  (let ((x (- (loc-x loc) (loc-x (agent-loc agent))))
	(y (- (loc-y loc) (loc-y (agent-loc agent)))))
    (case (agent-orientation agent)
      (  0 (make-loc :x (- y) :y (+ x))) ;changed
      ( 90 (make-loc :x x :y y)) ;ok
      (180 (make-loc :x (+ y) :y (- x)))
      (270 (make-loc :x (- x) :y (- y)))))) ;ok

;;;; Actions for Agents

(defun speak (agent world words)
  "Put the agent's words into the worlds list of sounds."
  ;; The sounds slot is an alist of (agent . words) pairs.
  (push (cons agent words) (world-sounds world)))

(defun turn (agent world direction)
  "Change the agent's orientation by turning."
  (declare (ignore world))
  (setf (agent-orientation agent)
	(add-degrees (agent-orientation agent)
		     (direction->degrees direction))))

(defun forward (agent world)
  "Move the agent to the location that is one step directly ahead of it.
  If the agent is in a nested grid, make the movement there."
  (declare (ignore world))
  (move-object agent
	       (location-towards (agent-loc agent) (agent-orientation agent)) 
	       (agent-world agent)))

(defun enter (agent world)
  "If there is a nested-world object in the agent's square, enter it."
  (let ((nested-object (find-object-if #'structured-object-p
				       (agent-loc agent) world)))
    (when nested-object
      (let ((nested-world (object-nested-world nested-object)))
	(push nested-object (agent-entered-stack agent))
	(setf (agent-world agent) nested-world)
	(setf *n* nested-world) ;???
	(setf (agent-loc agent) (world-start nested-world))
	;; Note that we put the agent in the nested world,
	;; but don't remove the agent from the outer world
	(place-object agent (world-start nested-world) nested-world)))))

(defun exit (agent world)
  "If the agent is in the start square of a nested world, exit from it."
  (when (and (agent-entered-stack agent)
	     (equal (agent-loc agent) (world-start world)))
    (let ((nested-object (pop (agent-entered-stack agent))))
      (setf (agent-world agent) (object-world nested-object))
      (setf (agent-loc agent) (object-loc nested-object)))))



