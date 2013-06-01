;; Here is the table-lookup-agent for Derron Simon (part 1 of assignment #1)
;;                                                  10/4/93
;;
;; It works by indexing into an 8x2 array of actions and state changes (indexing
;; by the current state and the dirt percept), the array looks like this:
;;
;;          state1    state2   state3   state4   state5   state6   state7   state8
;; dirt     suck ->1  suck->2  forw->4  suck->4  forw->6  suck->6  forw->7  off->8
;; no dirt  forw ->2  left->3  forw->4  left->5  forw->6  left->7  forw->7  off->8
;;
;; I simulate an array with a case statement with embedded if statements.
;;
;;
;; There are 10 possible environments containing 2 pieces of dirt total:
;;       (here I use 0,0 to refer to lower left, and 1,1 to refer to upper right)
;;    case 1  - 2 dirts in 0,0
;;    case 2  - 2 dirts in 1,0
;;    case 3  - 2 dirts in 1,1
;;    case 4  - 2 dirts in 0,1
;;    case 5  - 1 dirt in 0,0 - 1 dirt in 1,0
;;    case 6  - 1 dirt in 0,0 - 1 dirt in 1,1
;;    case 7  - 1 dirt in 0,0 - 1 dirt in 0,1
;;    case 8  - 1 dirt in 1,0 - 1 dirt in 1,1
;;    case 9  - 1 dirt in 1,0 - 1 dirt in 0,1
;;    case 10 - 1 dirt in 0,1 - 1 dirt in 1,1
;;
;;    for all these cases the table-lookup-agent has a performance score of
;;         190.00 average for TABLE-LOOKUP-AGENT
;;
;; There are 4 possible environments containing 1 piece of dirt:
;;
;;    case 1 - 1 dirt in 0,0
;;    case 2 - 1 dirt in 1,0
;;    case 3 - 1 dirt in 0,1
;;    case 4 - 1 dirt in 1,1
;;
;;    for all these cases the table-lookup-agent has a performance score of
;;         91.00 average for TABLE-LOOKUP-AGENT
;;
;; There is 1 possible environment containing 0 pieces of dirt:
;;
;;    case 1 - NO DIRT!
;;
;;    for this case the table-lookup-agent has a performance score of
;;         -9.00 average for TABLE-LOOKUP-AGENT
;;
;; The average case performance of TABLE-LOOKUP-AGENT is:
;;         150.33
;;

;; ----- CODE STARTS BELOW -----

;; (load "utilities.lisp")
;; (load "environment.lisp")
;; (load "vacuum.lisp")

(defun table-lookup-agent (name)
  (make-agent
   :name name
   :program
   (let ((state 1))
     #'(lambda (percept)
         (let ((bump (first percept))
               (dirt (second percept))
               (home (third percept)))
           (case state
               (1    (cond (dirt        'suck)
                           ((not dirt)  (incf state) 'forward)))
               (2    (cond (dirt        'suck)
                           ((not dirt)  (incf state) '(turn left))))
               (3    (incf state) 'forward)
               (4    (cond (dirt        'suck)
                           ((not dirt)  (incf state) '(turn left))))
               (5    (incf state) 'forward)
               (6    (cond (dirt        'suck)
                           ((not dirt)  (incf state) '(turn left))))
               (7    (cond (dirt        'suck)
                           ((not dirt)  (incf state) 'forward)))
               (8    `shut-off)))))))

(defun small-vacuum-world (&key (agents (list (table-lookup-agent 'T))))
  (initialize-world :agents agents :walled t
		    :objects '((dirt .5))
		    :x-size 4 :y-size 4
;; wall around outer edge leaves a 2x2 open environment
		    :score-fn #'vacuum-performance-fn
		    :percept-fn #'vacuum-world-percept))

