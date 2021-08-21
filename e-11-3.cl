#!/usr/bin/env sbcl --script

;
; # Chapter 11 – Exercise 3.
;
; > Suppose that a number of classes are defined as follows:
; >
; > ```text
; > (defclass a (c d) ...)      (defclass e () ...)
; > (defclass b (d c) ...)      (defclass f (h) ...)
; > (defclass c () ..)          (defclass g (h) ...)
; > (defclass d (e f g) ...)    (defclass h () ...)
; > ```
; >
; > (a) Draw the network representing the ancestors of a, and list the
; > classes an instance of a belongs to, from most to least specific.
; >
; > (b) Do the same for b.
;

(defun read-top-level-class-defs (&rest body)
  (read-top-level-class-defs-inner body))

(defun read-top-level-class-defs-inner (body)
  (let ((statement (car body)))
    (if statement
      (progn
        (princ statement)
        (terpri)
        (read-top-level-class-defs-inner (cdr body))))))

(read-top-level-class-defs
  '(defclass a (c d) ())
  '(defclass b (d c) ())
  '(defclass c () ())
  '(defclass d (e f g) ())
  '(defclass e () ())
  '(defclass f (h) ())
  '(defclass g (h) ())
  '(defclass h () ()))
