;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
#||
File:		vision.lisp
Author:		George J. Grevera
Description:	Support functions for the students for the computer vision
		assignment.

		Note:  File support is limited to grey pgm/pbm ascii files.
		This format was chosen only because it is simple and is
		supported by xv.  This format is as follows:

		P2		<- indicates file type
		cols rows	<- number of rows and cols in the image
		max-grey-value	<- you guessed what this is
		v1,1 v1,2 v1,3  . . . v1,cols
		.
		.
		.
		vrows,1 vrows,2 . . . vrows,cols
		
||#
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
(in-package "USER")
(proclaim '(optimize (compilation-speed 0)))
;---------------------------------------------------------------------------
; some global parameters . . .
(defparameter *print-array* t "print array contents")
(defparameter *print-length* 15 "stop after printing this many rows or cols")
;---------------------------------------------------------------------------
(defun image-rows (image)
  "return the # of rows in an image"
  (first (array-dimensions image)))
;---------------------------------------------------------------------------
(defun image-cols (image)
  "return the # of cols in an image"
  (second (array-dimensions image)))
;---------------------------------------------------------------------------
(defun load-image (name)
  (let ((in (open name
		  :direction :input
		  :if-does-not-exist :error))
	rows
	cols)
       (format t "loading ~A . . .~%" name)
       ; check the file type
       (assert (equal "P2" (read-line in)))
       ; skip any comment lines i.e. lines beginning with a #
       (let (c)
	    (loop
	        (setq c (read-char in))
	        (if (not (eq #\# c)) (return))
	        (read-line in))
	    (unread-char c in))
       (setq cols (read in))		; read in the number of cols & rows
       (setq rows (read in))
       (assert (>= (read in) 0))	; skip the max grey value field
       (format t "rows = ~D, cols = ~D~%" rows cols)
       ; load the image . . .
       (let ((image (make-array (list rows cols)
				:initial-element 0
				:element-type 'integer)))
	    (dotimes (r rows)
	        (dotimes (c cols)
		    (setf (aref image r c) (read in)))
		(format t "."))
	    (format t "~%done~%")
	    (close in)
	    ; return the image
	    image)))
;---------------------------------------------------------------------------
(defun save-image (name image max-val)
  (let ((out (open name
		   :direction :output
		   :if-exists :supersede))
	(rows (image-rows image))
	(cols (image-cols image))
	)
       (format out "P2~%")			; write the file type
       (format out "~D ~D~%" cols rows)		; write the size info
       (format out "~D~%" max-val)
       ; save the image . . .
       (dotimes (r rows)
		(dotimes (c cols)
			 (let ((val (aref image r c)))
			      (if (< val 0) (setq val 0))
			      (setq val (round val))
			      ; write the value to the file
			      (format out " ~D" val)))
		(format t ".")
		(format out "~%"))
       (close out)
       t))
;---------------------------------------------------------------------------
(defun random-image (rows cols)
  (let ((image (make-array (list rows cols))))
       (dotimes (r rows)
		(dotimes (c cols)
			 (setf (aref image r c) (random 10))))
       image))
;---------------------------------------------------------------------------
(defun random-binary-image (rows cols)
  (let ((image (make-array (list rows cols))))
       (dotimes (r rows)
		(dotimes (c cols)
			 (setf (aref image r c) (random 2))))
       image))
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
