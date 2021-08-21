#!/usr/bin/env sbcl --script

;
; # Chapter 11 – Exercise 2.
;
; > Rewrite the code in Figure 9.5 so that spheres and points are classes,
; > and intersect and normal are generic functions.
;

; Code from Figure 9.2, with modifications

(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defclass point ()
  ((x :accessor x
      :initarg :x
      :initform 0)
   (y :accessor y
      :initarg :y
      :initform 0)
   (z :accessor z
      :initarg :z
      :initform 0)))

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

; Code from Figure 9.3, with modifications

(defclass surface ()
  ((color :accessor color
          :initarg :color)))

(defparameter *world* nil)
(defconstant eye (make-instance 'point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
                       (unit-vector (- x (x eye))
                                    (- y (y eye))
                                    (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
      (* (lambert s int xr yr zr) (color s))
      0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

; Code from Figure 9.5, with modifications

(defclass sphere (point surface)
  ((radius :accessor sphere-radius
           :initarg :radius
           :initform 1)))

(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere
             :radius r
             :x x :y y :z z
             :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x s)) xr)
                             (* (- (y pt) (y s)) yr)
                             (* (- (z pt) (z s)) zr)))
                     (+ (sq (- (x pt) (x s)))
                        (sq (- (y pt) (y s)))
                        (sq (- (z pt) (z s)))
                        (- (sq (sphere-radius s)))))))
    (if n
      (make-instance 'point
        :x (+ (x pt) (* n xr))
        :y (+ (y pt) (* n yr))
        :z (+ (z pt) (* n zr))))))

(defmethod normal ((s sphere) (pt point))
  (unit-vector (- (x s) (x pt))
               (- (y s) (y pt))
               (- (z s) (z pt))))

; Code from Figure 9.6

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))

(ray-test 8)
