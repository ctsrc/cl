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

(defun populate-network-parents-table (&rest body)
  (let ((parents-netw (make-hash-table)))
    (class-def
      :cls 't
      :parents nil
      :with-network-parents-table parents-netw)
    (class-def
      :cls 'standard-object
      :parents '(t)
      :with-network-parents-table parents-netw)
    (populate-network-parents-table-inner
      :remaining-body body
      :with-network-parents-table parents-netw)
    parents-netw))

(defun populate-network-parents-table-inner (&key remaining-body with-network-parents-table)
  (if (not remaining-body)
    ()
    (let ((statement (car remaining-body)))
      (if (equal (car statement) 'defclass)
        (let ((cls (cadr statement))
              (parents (caddr statement))
              (cls-more (cdddr statement)))
          (if parents
            (class-def
              :cls cls
              :parents parents
              :with-network-parents-table with-network-parents-table)
            (class-def
              :cls cls
              :parents '(standard-object)
              :with-network-parents-table with-network-parents-table))))
      (populate-network-parents-table-inner
        :remaining-body (cdr remaining-body)
        :with-network-parents-table with-network-parents-table))))

(defun class-def (&key cls parents with-network-parents-table)
  (setf (gethash cls with-network-parents-table) parents))

(defun populate-network-children-table (&key for-root with-network-parents-table)
  (let ((children-netw (make-hash-table)))
       ()
       children-netw))

;(defun resolve-specifics-inner-populate (curr-child parents)
;  (let* ((curr-parent (car parents))
;         (currently-known-children (gethash curr-parent *classes-children*)))
;    (if curr-parent
;      (progn
;        (setf (gethash curr-parent *classes-children*) (cons curr-child currently-known-children))
;        (resolve-specifics-inner-populate curr-parent (gethash curr-parent *classes-parents*)))
;        (resolve-specifics-inner-populate curr-child (cdr parents)))))

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

(defparameter *classes-parents* nil)
(setf *classes-parents*
  (populate-network-parents-table
    '(defclass a (c d) ())
    '(defclass b (d c) ())
    '(defclass c () ())
    '(defclass d (e f g) ())
    '(defclass e () ())
    '(defclass f (h) ())
    '(defclass g (h) ())
    '(defclass h () ())))

(princ "hash-table *classes-parents*:")
(terpri)
(print-hash-table *classes-parents* 'princ)

; Subquestion a)

(defparameter *classes-children* nil)
(populate-network-children-table :with-root 'a :with-network-parents-table *classes-parents*)

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
