;
; CSE 391 Assignment #5 Part 1 for Derron Simon
;         
;     I implemented each part in the most straightforward way I could.  I
;     did not make use of the :equals operator, although I could have in
;     parts 3 and 4 - I found add and dele to be sufficient.
;

(load "~bonnie/cse391/snlp/variable")
(load "~bonnie/cse391/snlp/plan-utils")
(load "~bonnie/cse391/snlp/snlp")
(use-package 'plan-utils)
(setq PLAN-UTILS:*VERBOSE* t)

;
; Problem #1
;
; This problem isn't very extensible (ie. doesn't use SockOn and ShoeOn), but
; does work.  I had to change to a better representation for problems 2-4.
;

(defun morning-domain ()
  ;; purge old domain prior to defining a new domain
  (reset-domain)

  (defstep :action '(rightshoe)
    :precond '((rightsockon))
    :add '((rightshoeon)))

  (defstep :action '(leftshoe)
    :precond '((leftsockon))
    :add '((leftshoeon)))

  (defstep :action '(rightsock)
    :add '((rightsockon)))

  (defstep :action '(leftsock)
    :add '((leftsockon)))
)

(defun easy-morning ()
  (morning-domain)
  (snlp:plan '()
	     '((rightshoeon) (leftshoeon))
      ))

;
; Problem #2
;

(defun morning-domain2 ()
  ;; purge old domain prior to defining a new domain
  (reset-domain)

  (defstep :action '(PutOnShoe ?side)
    :precond '((SockOn ?side))
    :add '((ShoeOn ?side)))

  (defstep :action '(PutOnSock ?side)
    :add '((SockOn ?side)))
)

(defun easy-morning2 ()
  (morning-domain2)
  (snlp:plan '()
	     '((ShoeOn left) (ShoeOn right))
      ))

;
; Problem #3
;
; (harder-roach-morning) was interesting because it used to give the action:
;   (PROJECTILEROACH ?side)
; But adding (not (ShoeOn Right)) in the preconditions solved that.

(defun morning-domain3 ()
  ;; purge old domain prior to defining a new domain
  (reset-domain)

  (defstep :action '(PutOnShoe ?side)
    :precond '((SockOn ?side) (not (ShoeOn ?side)))
    :add '((ShoeOn ?side))
    :dele '((not (ShoeOn ?side))))

  (defstep :action '(PutOnSock ?side)
    :add '((SockOn ?side))
    :dele '((not (SockOn ?side))))

  (defstep :action '(StompRoach ?side)
    :precond '((ShoeOn ?side))
    :add '((Dead Roach)))

  (defstep :action '(ProjectileRoach ?side)
    :precond '((not (ShoeOn ?side)))
    :add '((Dead Roach)))
)

(defun hard-roach-morning ()
  (morning-domain3)
  (snlp:plan '((not (ShoeOn left)) (not (ShoeOn Right)) )
	     '((Dead Roach) (ShoeOn Left) (ShoeOn Right))
      ))

(defun harder-roach-morning ()
  (morning-domain3)
  (snlp:plan '((not (ShoeOn left)) (not (ShoeOn Right)) )
	     '((Dead Roach))
      ))

;
; Problem #4
;
; Here I add a ConfirmKillRoach action.  Ideally I think there should be an
; unnamed action instead that operates as an AND of the two actions Projectile
; and Stomp.  Perhaps an actions can be goals, but I had problems trying that.
;

(defun morning-domain4 ()
  ;; purge old domain prior to defining a new domain
  (reset-domain)

  (defstep :action '(PutOnShoe ?side)
    :precond '((SockOn ?side) (not (ShoeOn ?side)))
    :add '((ShoeOn ?side))
    :dele '((not (ShoeOn ?side))))

  (defstep :action '(PutOnSock ?side)
    :add '((SockOn ?side))
    :dele '((not (SockOn ?side))))

  (defstep :action '(StompRoach ?side)
    :precond '((ShoeOn ?side))
    :add '((Stomped Roach)))

  (defstep :action '(ProjectileRoach ?side)
    :precond '((not (ShoeOn ?side)))
    :add '((Projectiled Roach)))

  (defstep :action 'ConfirmKillRoach)
    :precond '((Projectiled Roach) (Stomped Roach))
    :add '((Dead Roach)))
)

(defun hard-superroach-morning ()
  (morning-domain4)
  (snlp:plan '((not (ShoeOn left)) (not (ShoeOn Right)) )
	     '((Dead Roach) (ShoeOn Left) (ShoeOn Right))
      ))

(defun harder-superroach-morning ()
  (morning-domain4)
  (snlp:plan '((not (ShoeOn left)) (not (ShoeOn Right)) )
	     '((Dead Roach))
      ))









