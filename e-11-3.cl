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
    (class-def 't nil)
    (class-def 'standard-object '(t))
    (read-top-level-class-defs-inner body)))

(defun read-top-level-class-defs-inner (body)
  (if (not body)
    ()
    (let ((statement (car body)))
      (if (equal (car statement) 'defclass)
        (let ((cls (cadr statement))
              (parents (caddr statement))
              (cls-more (cdddr statement)))
          (if parents
            (class-def cls parents)
            (class-def cls '(standard-object)))))
      (read-top-level-class-defs-inner (cdr body)))))

(defun class-def (cls parents)
  (setf (gethash cls *classes-parents*) parents))

(defparameter *classes-children* nil)

(defun resolve-specifics (query-cls)
  (progn
    (defparameter *classes-children* (make-hash-table))
    (resolve-specifics-inner-populate query-cls (gethash query-cls *classes-parents*))))

(defun resolve-specifics-inner-populate (curr-child parents)
  (princ curr-child)
  (princ " ")
  (princ parents)
  (terpri)
  (let* ((curr-parent (car parents))
         (currently-known-children (gethash curr-parent *classes-children*)))
    (if curr-parent
      (progn
        (setf (gethash curr-parent *classes-children*) (cons curr-child currently-known-children))
        (resolve-specifics-inner-populate curr-parent (gethash curr-parent *classes-parents*)))
        (resolve-specifics-inner-populate curr-child (cdr parents)))))

; Helper functions

; Based on https://stackoverflow.com/a/9729303
(defun print-hash-table (hash-table fun)
  (loop for key being the hash-keys of hash-table
        for value being the hash-values of hash-table
        do (princ key)
           (princ " => ")
           (princ value)
           (terpri)))

; Given body

(read-top-level-class-defs
  '(defclass a (c d) ())
  '(defclass b (d c) ())
  '(defclass c () ())
  '(defclass d (e f g) ())
  '(defclass e () ())
  '(defclass f (h) ())
  '(defclass g (h) ())
  '(defclass h () ()))

(princ "hash-table *classes-parents*:")
(terpri)
(princ (print-hash-table *classes-parents* 'princ))
(terpri)

; Subquestion a)

;(princ (resolve-specifics 'a))
;(terpri)
;;(princ *classes-children*)
;;(terpri)
;(princ (gethash 'a *classes-children*))
;(terpri)
;(princ (gethash 'c *classes-children*))
;(terpri)
;(princ (gethash 'd *classes-children*))
;(terpri)
;(princ (gethash 'e *classes-children*))
;(terpri)
;(princ (gethash 'f *classes-children*))
;(terpri)
;(princ (gethash 'g *classes-children*))
;(terpri)
;(princ (gethash 'h *classes-children*))
;(terpri)

; Subquestion b)

;(princ (resolve-specifics 'b))
;(terpri)
