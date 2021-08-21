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

(defparameter *classes-parents* nil)

(defun read-top-level-class-defs (&rest body)
  (progn
    (defparameter *classes-parents* (make-hash-table))
    (read-top-level-class-defs-inner body)))

(defun read-top-level-class-defs-inner (body)
  (let ((statement (car body)))
    (if (equal (car statement) 'defclass)
      (let ((cls (cadr statement))
            (parents (caddr statement))
            (cls-more (cdddr statement)))
        (class-def cls parents cls-more)
        (read-top-level-class-defs-inner (cdr body))))))

(defun class-def (cls parents class-more)
  (setf (gethash cls *classes-parents*) parents))

(read-top-level-class-defs
  '(defclass a (c d) ())
  '(defclass b (d c) ())
  '(defclass c () ())
  '(defclass d (e f g) ())
  '(defclass e () ())
  '(defclass f (h) ())
  '(defclass g (h) ())
  '(defclass h () ()))

(princ (gethash 'a *classes-parents*))
(terpri)
