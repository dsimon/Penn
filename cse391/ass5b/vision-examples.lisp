;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
#||
File:		vision-examples.lisp
Author:		George J. Grevera
Description:	Examples for the students for the computer vision
		assignment.
||#
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
(in-package "USER")
;---------------------------------------------------------------------------

; some sample masks . . .

(defconstant m1 (make-array '(3 3) :initial-contents
  '((-1 0 1)
    (-1 0 1)
    (-1 0 1))))

(defconstant m2 (make-array '(3 3) :initial-contents
  '(( 1  1  1)
    ( 0  0  0)
    (-1 -1 -1))))

(defconstant m3 (make-array '(3 3) :initial-contents
  '((0.1 0.1 0.1)
    (0.1 0.2 0.1)
    (0.1 0.1 0.1))))

(defconstant m4 (make-array '(3 3) :initial-contents
  '(( 0    -0.25  0)
    (-0.25  1    -0.25)
    ( 0    -0.25  0))))
;---------------------------------------------------------------------------

; some sample images . . .

(defconstant p1 (make-array '(18 15) :initial-contents
  '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 1 1 0 0 0 0 0 0)
    (0 0 0 0 0 1 0 1 0 1 0 0 0 0 0)
    (0 0 0 0 1 0 0 1 0 0 1 0 0 0 0)
    (0 0 0 1 0 0 0 1 0 0 0 1 0 0 0)
    (0 0 1 0 0 0 0 1 0 0 0 0 1 0 0)
    (0 0 1 0 0 0 0 1 0 0 0 0 1 0 0)
    (0 0 1 0 0 0 0 1 0 0 0 0 1 0 0)
    (0 0 1 0 0 0 0 1 0 0 0 0 1 0 0)
    (0 0 1 0 0 0 0 1 0 0 0 0 1 0 0)
    (0 0 1 0 0 0 0 1 0 0 0 0 1 0 0)
    (0 0 0 1 0 0 0 1 0 0 0 1 0 0 0)
    (0 0 0 0 1 0 0 1 0 0 1 0 0 0 0)
    (0 0 0 0 0 1 0 1 0 1 0 0 0 0 0)
    (0 0 0 0 0 0 1 1 1 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defconstant p2 (make-array '(1 1) :initial-contents
  '((1))))

(defconstant p3 (make-array '(1 5) :initial-contents
  '((1 1 1 1 1))))

(defconstant p4 (make-array '(1 5) :initial-contents
  '((9 9 9 9 9))))

(defconstant p5 (make-array '(5 5) :initial-contents
  '((1 0 0 0 0)
    (0 1 0 0 0)
    (0 0 1 0 0)
    (0 0 0 1 0)
    (0 0 0 0 1))))

(defconstant p6 (make-array '(5 5) :initial-contents
  '((1 0 0 0 0)
    (1 0 0 0 0)
    (1 0 0 0 0)
    (1 0 0 0 0)
    (1 1 1 1 1))))

(defconstant p7 (make-array '(2 5) :initial-contents
  '((1 1 1 1 1)
    (1 1 1 1 1))))

(defconstant p8 (make-array '(6 5) :initial-contents
  '((1 0 0 0 0)
    (1 1 0 0 0)
    (0 1 1 0 0)
    (0 0 1 1 0)
    (0 0 0 1 1)
    (0 0 0 0 1))))

(defconstant p9 (make-array '(2 2) :initial-contents
  '((1 1)
    (1 1))))

(defconstant p10 (make-array '(3 3) :initial-contents
  '((0 1 0)
    (1 1 1)
    (0 1 0))))

(defconstant p11 (make-array '(7 7) :initial-contents
  '((0 0 0 1 0 0 0)
    (0 0 1 1 1 0 0)
    (0 1 1 1 1 1 0)
    (1 1 1 1 1 1 1)
    (0 1 1 1 1 1 0)
    (0 0 1 1 1 0 0)
    (0 0 0 1 0 0 0))))

(defconstant p12 (make-array '(4 4) :initial-contents
  '((1 1 1 1)
    (1 1 1 1)
    (1 1 1 1)
    (1 1 1 1))))

(defconstant p13 (make-array '(4 7) :initial-contents
  '((0 0 0 1 0 0 0)
    (0 0 1 1 1 0 0)
    (0 1 1 1 1 1 0)
    (1 1 1 1 1 1 1))))
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
