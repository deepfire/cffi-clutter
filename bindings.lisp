(in-package :cffi-clutter)

(declaim (inline g-type-from-class g-type-from-instance))
(defun g-type-from-class (class)
  (foreign-slot-value
   class
   'g-type-class 'g-type))

(defun g-type-from-instance (instance)
  (g-type-from-class (foreign-slot-value instance 'g-type-instance 'g-class)))
