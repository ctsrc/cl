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

(defun make-network-parents-table (&rest body)
  (let ((parents-netw (make-hash-table)))
    (class-def
      :cls 't
      :parents nil
      :with-network-parents-table parents-netw)
    (class-def
      :cls 'standard-object
      :parents '(t)
      :with-network-parents-table parents-netw)
    (populate-network-parents-table
      :remaining-body body
      :with-network-parents-table parents-netw)
    parents-netw))

(defun populate-network-parents-table (&key remaining-body with-network-parents-table)
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
      (populate-network-parents-table
        :remaining-body (cdr remaining-body)
        :with-network-parents-table with-network-parents-table))))

(defun class-def (&key cls parents with-network-parents-table)
  (setf (gethash cls with-network-parents-table) parents))

(defun make-network-children-table (&key with-root with-network-parents-table)
  (let ((children-netw (make-hash-table)))
       (populate-network-children-table
         :curr-child with-root
         :parents (gethash with-root with-network-parents-table)
         :with-network-parents-table with-network-parents-table
         :with-network-children-table children-netw)
       children-netw))

(defun populate-network-children-table (&key curr-child parents with-network-parents-table with-network-children-table)
  (if parents
    (let* ((curr-parent (car parents))
           (currently-known-children (gethash curr-parent with-network-children-table)))
      (if (not (member curr-child currently-known-children))
        (progn
          (populate-network-children-table
            :curr-child curr-child
            :parents (cdr parents)
            :with-network-parents-table with-network-parents-table
            :with-network-children-table with-network-children-table)
          (populate-network-children-table
            :curr-child curr-parent
            :parents (gethash curr-parent with-network-parents-table)
            :with-network-parents-table with-network-parents-table
            :with-network-children-table with-network-children-table)
          (setf (gethash curr-parent with-network-children-table) (cons curr-child currently-known-children))
          ())))))

; Helper functions

; Based on https://stackoverflow.com/a/9729303
(defun print-hash-table (hash-table)
  (loop for key being the hash-keys of hash-table
        for value being the hash-values of hash-table
        do (princ key)
           (princ " => ")
           (princ value)
           (terpri)))

; Given body

(defparameter *classes-parents* nil)
(setf *classes-parents*
  (make-network-parents-table
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
(print-hash-table *classes-parents*)
(terpri)

; Subquestion a)

(defparameter *classes-children* nil)
(setf *classes-children*
  (make-network-children-table
    :with-root 'a
    :with-network-parents-table *classes-parents*))

(princ "hash-table *classes-children*:")
(terpri)
(print-hash-table *classes-children*)

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
