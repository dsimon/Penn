;
; Here's the code for assignment #5 part b for Derron Simon
;
; I wrote George on 12/18 asking if we needed to hand in examples, 
; but haven't received a reply.  I assume he'll be testing these, so
; I left out the examples, but I can provide a transcript of my editing
; sessions on request.
;
; Everything works fine in my tests.  I even ran a lot through xv over a
; 14.4k bps SLIP connection to test them out!
;


(load "vision-example")
(load "vision")

(defconstant 2p (make-array '(4 4) :initial-contents
  '((1 1 1 1)
    (4 1 1 -1)
    (1 1 7.5 1)
    (1 1 1 1))))

(defun get-max-value (image)
  (setq maxi 0)
  (loop for i from 0 to (- (image-rows image) 1)
	do (loop for j from 0 to (- (image-cols image) 1)
	      do (when (> (aref image i j) maxi)
		    (setq maxi (aref image i j))
		    )
	      )
	)
  (+ (round maxi) 0)              ; this is to remove second result of round
  )

(defun histogram (image)
  (setq hist (make-array (+ (get-max-value image) 1) 
			 :initial-element 0 
			 :element-type 'integer))
  (dotimes (i (- (image-rows image) 1))
	(dotimes (j (- (image-cols image) 1))
		 (cond ((>= (aref image i j) 0) 
		       (incf (aref hist (round (aref image i j)))))
		       (t (incf (aref hist 0)))
		       )
		 )
	)
  hist
  )

(defun threshold (a i j rows cols tval)
  ; the threshold value is tval
  ; rows & cols should always be 1
  (assert (and (eq rows 1) (eq cols 1)))
  (if (>= (aref a i j) tval)
      1
      0))

;
; process basically makes a new array that's 2*op-col wide and 2*op-row long,
; this fixes the problem of using the operations on the edges even for
; non-centered masks (ie. the origin of the mask can be upper-left).
;

(defun process (im op op-rows op-cols &optional op-arg)
  (let (A B i j imax jmax)
    (setq imax (first (array-dimensions im)))
    (setq jmax (second (array-dimensions im)))
    (setf A (make-array (list (+ imax (* op-rows 2)) (+ jmax (* op-cols 2))) 
			:initial-element 0
			:element-type 'integer))
    (setf B (make-array (list imax jmax)
			:initial-element 0
			:element-type 'integer))
    (dotimes (i imax)
	     (dotimes (j jmax)
		      (setf (aref A (+ i op-rows) (+ j op-cols)) 
			    (aref im i j))))
    (dotimes (i imax B)
	     (dotimes (j jmax)
		      (setf (aref B i j)
			    (funcall op A (+ i op-rows) (+ j op-cols) 
				     op-rows op-cols op-arg))))))

(defun median (a i j rows cols &rest ignored)
  (let (alist blist r c n)
    (dotimes (r rows)
	     (dotimes (c cols)
		      (setq alist
			    (cons (aref a (+ i (round (- 0 (/ rows 2))) r 1)
					  (+ j (round (- 0 (/ cols 2))) c 1)) 
				  alist))))
    (setq blist (sort alist '<=))
    (nth (floor (/ (* rows cols) 2)) blist)
    )
)

(defun edge (a i j rows cols &rest ignored)	     
  (assert (and (eq rows 2) (eq cols 2)))
  (round (sqrt (+ (expt (- (aref a (+ i 1) (+ j 1)) (aref a i j)) 2)
	   (expt (- (aref a i (+ j 1)) (aref a (+ i 1) j)) 2)))))

(defun convolve (a i j rows cols mask)
  (let (r c tot)
    (setq tot 0)
    (dotimes (r rows)
	     (dotimes (c cols)
		(setq tot (+ tot (* (aref a (+ i r) (+ j c)) (aref mask r c))))
		))
    tot))

; binary-erode works, here's the output:
;
; #P"/home3/d/dsimon/cse391/ass5b/vis.lisp"
; > (setq out (process p11 #'binary-erode 2 2 p9))
; (setq out (process p11 #'binary-erode 2 2 p9))
; #2A((0 0 0 0 0 0 0) (0 0 1 1 0 0 0) (0 1 1 1 1 0 0) (0 1 1 1 1 0 0) \
; (0 0 1 1 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0))

(defun binary-erode (a i j rows cols mask)
  (let (r c result)
    (setq result 1)
    (dotimes (r rows)
	     (dotimes (c cols)
		      (when (not (equalp (aref mask r c) (aref a (+ i r) (+ j c))))
			    (setq result 0))
		      )
	     )
    result
    )
  )

; binary-dilate also works, here's an example
;
; > (setq out (process p11 #'binary-dilate 2 2 p9))
; #2A((0 1 1 1 1 0 0) (1 1 1 1 1 1 0) (1 1 1 1 1 1 1) (1 1 1 1 1 1 1) \
; (1 1 1 1 1 1 0) (0 1 1 1 1 0 0) (0 0 1 1 0 0 0))
; >
 
(defun binary-dilate (a i j rows cols mask)
  (let (r c result)
    (setq result 0)
    (dotimes (r rows)
	     (dotimes (c cols)
		      (when (equalp (aref mask r c) (aref a (+ i r) (+ j c)))
			    (setq result 1))
		      )
	     )
    result
    )
  )





