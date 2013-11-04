(in-package :cffi-clutter)

;; mid-level bindings
;; I don't think there is much point in wrapping pointers with objects at this level

(defun event-get-coords (event)
  (c-with ((x :float)
           (y :float))
    (clutter-event-get-coords event (x &) (y &))
    (list x y)))

(defun set-color (color r g b &optional (a (c-ref color clutter-color :alpha)))
  (setf (c-ref color clutter-color :red)   r
        (c-ref color clutter-color :green) g
        (c-ref color clutter-color :blue)  b
        (c-ref color clutter-color :alpha) a)
  color)

(defun get-color (color)
  (list (c-ref color clutter-color :red)
        (c-ref color clutter-color :green)
        (c-ref color clutter-color :blue)
        (c-ref color clutter-color :alpha)))

(defmacro with-color ((var red green blue &optional (alpha 255)) &body body)
  `(let ((,var (clutter-color-new ,red ,green ,blue ,alpha)))
     (unwind-protect (progn ,@body)
       (clutter-color-free ,var))))

(defmacro with-colors (color-specs &body body)
  (if (cdr color-specs)
      `(with-color ,(car color-specs)
         (with-colors ,(cdr color-specs) ,@body))
      `(with-color ,(car color-specs)
         ,@body)))

(defvar *clutter-initialized* nil)

(defvar *clutter-initialization-addons* nil)

(defun init-clutter (&key (clutter-argument-list nil))
  (unless *clutter-initialized*
    (setf *clutter-initialized* t)
    (let ((result
           (if clutter-argument-list
               (let ((argc (length clutter-argument-list))
                     (argvs (mapcar #'foreign-string-alloc clutter-argument-list)))
                 (with-foreign-objects ((argc-pointer :int)
                                        (argv-pointer :pointer argc))
                   (loop for p in argvs
                      for i from 0
                      do (setf (mem-aref argv-pointer :pointer i) p))
                   (setf (mem-ref argc-pointer :int) argc)
                   (unwind-protect
                        (clutter-init argc-pointer argvs)
                     (mapc #'foreign-string-free argvs))))
               (with-foreign-object (argc :int)
                 (setf (mem-ref argc :int) 0)
                 (clutter-init argc (null-pointer))))))
      (dolist (symbol *clutter-initialization-addons*)
        (funcall symbol))
      (values result))))

;;;
;;; Idle events & support callback machinery
;;;
;; wrap callbacks in dispatcher function to enable use of lisp functions
;; signals are disconnected when objects are freed
;; note that signals connected to stage just pile up if not disconnected, since the default stage
;; is created once on init-clutter

(defun register-lisp-callback (lisp-callback c-dispatch)
  (let ((rid (foreign-alloc :uint64)))
    (register-resource (cons lisp-callback c-dispatch) rid)
    rid))

(declaim (inline call-lisp-callback))
(defun call-lisp-callback-by-rid (user-data &rest arguments)
  (destructuring-bind (lisp-callback . c-dispatch) (resource user-data)
    (declare (ignore c-dispatch))
    (apply lisp-callback arguments)))

(defun unregister-lisp-callback-by-rid (rid-ptr)
  (unregister-resource rid-ptr)
  (foreign-free rid-ptr)
  (values))

(defcallback source-callback gboolean ((data :pointer))
  (if (call-lisp-callback-by-rid data)
      (const "TRUE")
      (const "FALSE")))

(defcallback destroy-notify-callback :void ((data :pointer))
  (unregister-lisp-callback-by-rid data))

(defun add-idle (idle-function &key (priority (const "G_PRIORITY_DEFAULT_IDLE")))
  (let ((rid (register-lisp-callback idle-function (callback 'source-callback))))
    (clutter-threads-add-idle-full priority
                                   (callback 'source-callback)
                                   rid
                                   (callback 'destroy-notify-callback))))

(defun main-with-cleanup (stage)
  "Execute main loop, and when it ends remove everything from stage, disconnect all stage lisp signals, cleanup current pool and hide the stage."
  (clutter-main)
  (add-idle (lambda ()
              (clutter-main-quit)
              nil))
  (clutter-actor-remove-all-children stage)
  #+nil
  (disconnect-lisp-signals stage)
  #+nil
  (cleanup-pool *current-pool*)
  (clutter-actor-hide stage)
  (clutter-main))

;; if threads are not initialized %threads-enter/leave are a noop on C level
;; it doesn't need to be called in callbacks or threads-idle etc.
(defmacro with-clutter-lock (&body body)
  `(progn
     (clutter-threads-enter)
     (unwind-protect (progn ,@body)
       (clutter-threads-leave))))

(defun animation-mode (mode)
  (if (keywordp mode)
      (foreign-enum-value 'clutter-animation-mode mode)
      mode))

#+nil
(defun alpha-set-mode (alpha mode)
  (clutter-alpha-set-mode alpha (clutter-animation-mode mode)))

#+nil
(defun make-behaviour-path-with-knots (alpha &rest knots-xy)
  (assert (zerop (mod (length knots-xy) 2)))
  (let ((n (/ (length knots-xy) 2)))
    (c-let ((knots clutter-knot :count n))
      (loop for (x y . nil) on knots-xy by #'cddr
         for i from 0
         do (let ((knot (c-ref knots clutter-knot i)))
              (setf (c-ref (knots &) (autowrap:ptr clutter-knot) i :x) x
                    (c-ref (knots &) (autowrap:ptr clutter-knot) i :y) y)))
      (clutter-behaviour-path-new-with-knots alpha knots n))))

(defun actor-get-preferred-size (actor)
  (with-foreign-objects ((min-width :float)
                         (min-height :float)
                         (natural-width :float)
                         (natural-height :float))
    (clutter-actor-get-preferred-size actor min-width min-height natural-width natural-height)
    (values (mem-ref min-width :float) (mem-ref min-height :float)
            (mem-ref natural-width :float) (mem-ref natural-height :float))))

(defun actor-get-preferred-width (actor for-height)
  (with-foreign-objects ((min-width :float)
                         (natural-width :float))
    (clutter-actor-get-preferred-width actor for-height min-width  natural-width)
    (values (mem-ref min-width :float)
            (mem-ref natural-width :float))))

(defun actor-get-preferred-height (actor for-width)
  (with-foreign-objects ((min-height :float)
                         (natural-height :float))
    (clutter-actor-get-preferred-height actor for-width min-height  natural-height)
    (values (mem-ref min-height :float)
            (mem-ref natural-height :float))))

(defun actor-get-size (actor)
  (with-foreign-objects ((width :float)
                         (height :float))
    (clutter-actor-get-size actor width height)
    (values (mem-ref width :float)
            (mem-ref height :float))))

(defun actor-get-position (actor)
  (with-foreign-objects ((x :float)
                         (y :float))
    (clutter-actor-get-position actor x y)
    (values (mem-ref x :float)
            (mem-ref y :float))))

(defun stage-get-actor-at-position (stage pick-mode x y)
  (let ((result (clutter-stage-get-actor-at-pos stage pick-mode x y)))
    (if (null-pointer-p (ptr result))
        nil
        result)))

#+nil
(defun actor-get-geometry (actor)
  (c-with ((geometry clutter-geometry))
    (clutter-actor-get-geometry actor (geometry &))
    (list (c-ref geometry clutter-geometry :x)
          (c-ref geometry clutter-geometry :y)
          (c-ref geometry clutter-geometry :width)
          (c-ref geometry clutter-geometry :height))))

(defmacro with-perspective ((var fovy aspect z-near z-far) &body body)
  (with-gensyms (tmp)
    `(c-with ((,tmp clutter-perspective))
       (setf (c-ref ,tmp clutter-perspective :fovy) ,fovy
             (c-ref ,tmp clutter-perspective :aspect) ,aspect
             (c-ref ,tmp clutter-perspective :z-near) ,z-near
             (c-ref ,tmp clutter-perspective :z-far) ,z-far)
       (symbol-macrolet ((,var (,tmp &)))
         ,@body))))

(defun set-perspective (perspective f a near far)
  (setf (c-ref perspective clutter-perspective :fovy) f
        (c-ref perspective clutter-perspective :aspect) a
        (c-ref perspective clutter-perspective :z-near) near
        (c-ref perspective clutter-perspective :z-far) far)
  perspective)

(defun get-perspective (perspective)
  (list (c-ref perspective clutter-perspective :fovy)
        (c-ref perspective clutter-perspective :aspect)
        (c-ref perspective clutter-perspective :z-near)
        (c-ref perspective clutter-perspective :z-far)))

(defun get-stage-perspective (stage)
  "Wrapper around stage-get-perspective"
  (c-with ((perspective clutter-perspective))
    (clutter-stage-get-perspective stage (perspective &))
    (get-perspective (perspective &))))

(defun set-stage-perspective (stage fovy aspect z-near z-far)
  "Wrapper around stage-set-perspective"
  (with-perspective (perspective fovy aspect z-near z-far)
    (clutter-stage-set-perspective stage perspective)))
