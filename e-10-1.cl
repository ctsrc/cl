#!/usr/bin/env sbcl --script

;
; # Chapter 10 – Exercise 1.
;
; > If x is a, y is b, and z is (c d), write backquoted expressions
; > containing only variables that yield each of the following:
; >
; > (a) ((C D) A Z)
; > (b) (X B C D)
; > (c) ((C D A) Z)
;

(let ((x 'a)
      (y 'b)
      (z '(c d)))
     (princ `(,z ,x z))
     (terpri)
     (princ `(x ,y ,@z))
     (terpri)
     (princ `((,@z ,x) z))
     (terpri))
