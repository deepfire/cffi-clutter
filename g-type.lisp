(in-package :cffi-clutter)

;;;
;;; GType machinery
;;;
(defparameter *g-name-type-map* (alist-hash-table
                                 `(("gboolean"               . gboolean)
                                   ("guchar"                 . guchar)
                                   ("gint"                   . gint)
                                   ("guint"                  . guint)
                                   ("gulong"                 . gulong)
                                   ("gfloat"                 . :float)
                                   ("gdouble"                . :double)
                                   ("gpointer"               . :pointer)
                                   ("ClutterActor"           . clutter-actor)
                                   ("ClutterGravity"         . clutter-gravity)
                                   ("ClutterRequestMode"     . clutter-request-mode)
                                   ("ClutterRotateAxis"      . clutter-rotate-axis)
                                   ("ClutterRotateDirection" . clutter-rotate-direction)
                                   ("ClutterTextureQuality"  . clutter-texture-quality)
                                   ("CoglPixelFormat"        . cogl-pixel-format)
                                   ("PangoAlignment"         . pango-alignment)
                                   ("PangoEllipsizeMode"     . pango-ellipsize-mode)
                                   ("PangoWrapMode"          . pango-wrap-mode))
                                 :test 'equal))

(defparameter *g-type-name-map* (alist-hash-table (mapcar (lambda (acons) (cons (cdr acons) (car acons)))
                                                          (hash-table-alist *g-name-type-map*))))

(defparameter *g-type-to-ffi-cache* (make-hash-table))

;; string -> symbol
(defun g-name-to-ffi-type (g-name)
  (or (%g-name-to-ffi-type g-name)
      (error "~@<Unknown GObject type name: ~S.~:@>" g-name)))

(defun %g-name-to-ffi-type (g-name)
  (gethash g-name *g-name-type-map*))

;; symbol -> string
(defun ffi-type-to-g-name (ffi-type)
  (or (gethash ffi-type *g-type-name-map*)
      (error "~@<Unknown FFI type: ~S.~:@>" ffi-type)))

;; (or string symbol) -> integer
(defun g-type-lookup (name)
  (with-foreign-string (str (etypecase name
                              (string name)
                              (symbol (ffi-type-to-g-name name))))
    (g-type-from-name str)))

;; integer -> boolean
(defun g-type-known (g-type)
  (let ((type-name (foreign-string-to-lisp (g-type-name g-type))))
    (values (%g-name-to-ffi-type type-name)
            type-name)))

;; integer -> symbol
(defun g-type-to-ffi-type (g-type)
  (or (gethash g-type *g-type-to-ffi-cache*)
      (setf (gethash g-type *g-type-to-ffi-cache*)
            (g-name-to-ffi-type (foreign-string-to-lisp (g-type-name g-type))))))

;;;
;;; Runtime type definition
;;;
(defmacro define-g-type ((lisp-name c-name c-parent-name &optional (flags 0))
                                                           instance-fields
                                                           class-fields
                         &body body)
  ;; Requires implementation of:
  ;;
  ;;  INIT-<lisp-name>
  ;;  INITIALIZE-CLASS-<lisp-name>
  (declare (string c-name c-parent-name))
  (let* ((pkg *package*)
         (type-class-name      (format-symbol pkg "~A-CLASS"              lisp-name))
         (parent-class         (format-symbol pkg "*PARENT-CLASS-OF-~A*"  lisp-name))
         (type-id              (format-symbol pkg "*TYPE-ID-~A*"          lisp-name))
         (type-c-name          (format-symbol pkg "*TYPE-NAME-~A*"        lisp-name))
         ;;
         (init-cb              (format-symbol pkg "%INIT-~A"              lisp-name))
         (class-intern-init-cb (format-symbol pkg "%CLASS-INTERN-INIT-~A" lisp-name))
         ;;
         (init-fn              (format-symbol pkg "INIT-~A"               lisp-name))
         (class-init-fn        (format-symbol pkg "INITIALIZE-CLASS-~A"   lisp-name))
         (get-type-fn          (format-symbol pkg "GET-TYPE-~A"           lisp-name)))
    (with-gensyms (name-fstr parent-name-fstr)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (autowrap::define-foreign-record ',type-class-name
               :struct 0 0
               '((:class clutter-actor-class)
                 ,@class-fields))
           (autowrap::define-foreign-record ',lisp-name
               :struct 0 0
               '((:actor clutter-actor)
                 ,@instance-fields)))
         (defvar ,parent-class (cffi:make-pointer 0))
         (defvar ,type-id nil)
         (defvar ,type-c-name ,c-name)
         (defcallback ,class-intern-init-cb :void ((klass (:pointer ,type-class-name)))
           (setf ,parent-class (g-type-class-peek-parent klass))
           (,class-init-fn klass))
         (defcallback ,init-cb :void ((instance (:pointer ,lisp-name)))
           (,init-fn instance))
         (defun ,get-type-fn ()
           ;; XXX: protect from simultaneous init by different threads
           (unless ,type-id
             (setf ,type-id (g-type-register-static-simple
                             (with-foreign-string (,parent-name-fstr ,c-parent-name)
                               (g-type-from-name ,parent-name-fstr))
                             (with-foreign-string (,name-fstr ,c-name)
                               (g-intern-string ,name-fstr))
                             (foreign-type-size ',type-class-name)
                             (callback ',class-intern-init-cb)
                             (foreign-type-size ',lisp-name)
                             (callback ',init-cb)
                             ,flags))
             ,@body)
           ,type-id)))))

;; #define _G_DEFINE_TYPE_EXTENDED_BEGIN(TypeName, type_name, TYPE_PARENT, flags) 
;; 
;; static void     type_name##_init              (TypeName        *self); 
;; static void     type_name##_class_init        (TypeName##Class *klass); 
;; static gpointer type_name##_parent_class = NULL; 
;; static void     type_name##_class_intern_init (gpointer klass) 
;; { 
;;   type_name##_parent_class = g_type_class_peek_parent (klass); 
;;   type_name##_class_init ((TypeName##Class*) klass); 
;; } 
;; 
;; GType 
;; type_name##_get_type (void) 
;; { 
;;   static volatile gsize g_define_type_id__volatile = 0; 
;;   if (g_once_init_enter (&g_define_type_id__volatile))  
;;     { 
;;       GType g_define_type_id = 
;;         g_type_register_static_simple (TYPE_PARENT, 
;;                                        g_intern_static_string (#TypeName), 
;;                                        sizeof (TypeName##Class), 
;;                                        (GClassInitFunc) type_name##_class_intern_init, 
;;                                        sizeof (TypeName), 
;;                                        (GInstanceInitFunc) type_name##_init, 
;;                                        (GTypeFlags) flags); 
;;       { /* custom code follows */
;; #define _G_DEFINE_TYPE_EXTENDED_END()	
;;         /* following custom code */	
;;       }					
;;       g_once_init_leave (&g_define_type_id__volatile, g_define_type_id); 
;;     }					
;;   return g_define_type_id__volatile;	
;; } /* closes type_name##_get_type() */
