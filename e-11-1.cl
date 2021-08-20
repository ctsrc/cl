#!/usr/bin/env sbcl --script

;
; # Chapter 11 – Exercise 1.
;
; > Define accessors, initforms, and initargs for the classes defined
; > in Figure 11.2. Rewrite the associated code so that it no longer
; > calls slot-value.
;

(defclass rectangle ()
  ((height :accessor rectangle-height
           :initarg :height
           :initform 1)
   (width  :accessor rectangle-width
           :initarg :width
           :initform 1)))

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)))

(defmethod area ((x rectangle))
  (* (rectangle-height x) (rectangle-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

(princ (let ((r (make-instance 'rectangle :height 2 :width 3)))
         (area r)))
(terpri)
