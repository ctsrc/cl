#!/usr/bin/env sbcl --script

;
; # Chapter 11 – Exercise 1.
;
; > Define accessors, initforms, and initargs for the classes defined
; > in Figure 11.2. Rewrite the associated code so that it no longer
; > calls slot-value.
;

(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(princ (let ((r (make-instance 'rectangle)))
         (setf (slot-value r 'height) 2
               (slot-value r 'width)  3)
         (area r)))
(terpri)
