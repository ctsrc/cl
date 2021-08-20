#!/usr/bin/env sbcl --script

;
; # Chapter 11 – Exercise 1.
;
; > Define accessors, initforms, and initargs for the classes defined
; > in Figure 11.2. Rewrite the associated code so that it no longer
; > calls slot-value.
;

(defclass rectangle ()
  ((height :accessor rectangle-height)
   (width  :accessor rectangle-width)))

(defclass circle ()
  ((radius :accessor circle-radius)))

(defmethod area ((x rectangle))
  (* (rectangle-height x) (rectangle-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

(princ (let ((r (make-instance 'rectangle)))
         (setf (rectangle-height r) 2
               (rectangle-width  r) 3)
         (area r)))
(terpri)
