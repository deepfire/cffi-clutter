(in-package :cffi-clutter)

;; g-value -> symbol
(defun g-value-type (g-value &key (if-type-unknown :error))
  (g-type-to-ffi-type (%g-value-type g-value)))

(defun %g-value-type (g-value)
  (c-ref g-value g-value :g-type))

;;;
;;; GValues proper
;;;
(defconstant +sizeof-gvalue+ (foreign-type-size 'g-value))

(defmethod print-object ((o g-value) stream)
  (print-unreadable-object (o stream)
    (let ((g-type (%g-value-type o)))
      (multiple-value-bind (known c-name) (g-type-known g-type)
        (if known
            (format stream "GValue/~A value: ~S" (g-value-type o) (g-value o))
            (format stream "GValue/Unknown (#x~X/~S)" g-type c-name))))))

(defun zero-g-value (value)
  (loop for i from 0 below +sizeof-gvalue+
     do (setf (mem-aref (inc-pointer (ptr value) i) :uint8) 0)))

(defun initialize-g-value (value g-type)
  (zero-g-value value)
  (g-value-init value (g-type-lookup g-type))
  value)

(defun (setf g-value) (new-value g-value &key init-as-type)
  (let ((ffi-type (or init-as-type
                      (g-type-to-ffi-type (c-ref g-value g-value :g-type)))))
    (when init-as-type
      (initialize-g-value g-value ffi-type))
    (ecase ffi-type
      ((request-mode cogl-pixel-format pango-wrap-mode
                     pango-alignment pango-ellipsize-mode clutter-texture-quality
                     clutter-rotate-direction clutter-rotate-axis clutter-gravity)
       (g-value-set-enum g-value new-value))
      (gboolean (g-value-set-boolean g-value new-value))
      (gchar (g-value-set-char g-value new-value))
      (guchar (g-value-set-uchar g-value new-value))
      (gint (g-value-set-int g-value new-value))
      (guint (g-value-set-uint g-value new-value))
      (glong (g-value-set-long g-value new-value))
      (gulong (g-value-set-ulong g-value new-value))
      (gint64 (g-value-set-int64 g-value new-value))
      (guint64 (g-value-set-uint64 g-value new-value))
      (:float (g-value-set-float g-value new-value))
      (:double (g-value-set-double g-value new-value))
      (:string (g-value-set-string g-value new-value))
      (:pointer (g-value-set-pointer g-value new-value)))))

(defun g-value (g-value)
  (let ((ffi-type (g-value-type g-value)))
    (ecase ffi-type
      ((request-mode cogl-pixel-format pango-wrap-mode
                     pango-alignment texture-quality pango-ellipsize-mode
                     rotate-direction rotate-axis gravity)
       (enum-key ffi-type (g-value-get-enum g-value)))
      (gboolean (g-value-get-boolean g-value))
      (gchar (g-value-get-char g-value))
      (guchar (g-value-get-uchar g-value))
      (gint (g-value-get-int g-value))
      (guint (g-value-get-uint g-value))
      (glong (g-value-get-long g-value))
      (gulong (g-value-get-ulong g-value))
      (gint64 (g-value-get-int64 g-value))
      (guint64 (g-value-get-uint64 g-value))
      (:float (g-value-get-float g-value))
      (:double (g-value-get-double g-value))
      (:string (g-value-get-string g-value))
      (:pointer (g-value-get-pointer g-value)))))

(defun make-g-value (g-type &optional (value 0.0 value-supplied-p))
  (c-let ((gval g-value))
    (initialize-g-value gval g-type)
    (when value-supplied-p
      (setf (g-value gval) value))
    gval))

(defun free-g-value (g-value)
  (g-value-unset g-value)
  (autowrap:free g-value))

(defmacro with-g-value ((g-value g-type &optional (value 0.0 value-supplied-p)) &body body)
  `(let ((,g-value ,(if value-supplied-p
                        `(make-g-value ,g-type ,value)
                        `(make-g-value ,g-type))))
     (unwind-protect
          (progn ,@body)
       (free-g-value ,g-value))))
