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

(defun class-def (cls parents cls-more)
  (setf (gethash cls *classes-parents*) parents))

(defparameter *classes-children* nil)

(defun resolve-specifics (query-cls)
  (progn
    (defparameter *classes-children* (make-hash-table))
    (resolve-specifics-inner-populate query-cls (gethash query-cls *classes-parents*))
    (cons query-cls (resolve-specifics-inner-walk query-cls (gethash query-cls *classes-parents*)))))

(defun resolve-specifics-inner-populate (curr-child parents)
  (if parents
    (let* ((curr-parent (car parents))
           (currently-known-children (gethash curr-parent *classes-children*)))
      (if currently-known-children
          (setf (gethash curr-parent *classes-children*) (cons currently-known-children (cons curr-child nil)))
          (setf (gethash curr-parent *classes-children*) (cons curr-child nil)))
      (resolve-specifics-inner-populate curr-parent (gethash curr-parent *classes-parents*))
      (resolve-specifics-inner-populate curr-child (cdr parents)))))

(defun resolve-specifics-inner-walk (curr-child parents)
  (princ (list "curr-child" curr-child "parents" parents))
  (terpri)
  (if parents
    (let* ((curr-parent (car parents))
           (other-parents (cdr parents))
           (other-parents-walked (resolve-specifics-inner-walk curr-child  other-parents)))
      (if (is-last-child? curr-child (gethash curr-parent *classes-children*))
        (progn
          (princ "  is-last-child")
          (terpri)
          (let ((curr-parent-walked (resolve-specifics-inner-walk curr-parent (gethash curr-parent *classes-parents*))))
            (if curr-parent-walked
              (cons curr-parent-walked other-parents-walked)
              other-parents-walked)))
        (progn
          (princ "  not-last-child")
          (terpri)
          other-parents-walked)))))

(defun is-last-child? (curr-child children-of-parent)
  (princ "    ")
  (princ (list "curr-child" curr-child "children-of-parent" children-of-parent))
  (terpri)
  (if children-of-parent
    (let ((cmp-child (car children-of-parent))
          (rem-children (cdr children-of-parent)))
    (if (and (equal curr-child cmp-child) (not rem-children))
      t
      (is-last-child? curr-child rem-children)))))

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

; Subquestion a)

(princ (resolve-specifics 'a))
(terpri)
;(princ *classes-children*)
;(terpri)
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

(princ (resolve-specifics 'b))
(terpri)
