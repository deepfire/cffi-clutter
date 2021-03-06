(in-package :clutter-examples)

(defun create-content-actor ()
  (let ((content (clutter-actor-new))
        (pixbuf (gdk-pixbuf-new-from-file
                 (cl:namestring (asdf:system-relative-pathname
                                 (asdf:find-system :cffi-clutter)
                                 "examples/data/redhand.png"))
                 (make-pointer 0)))
        (image (clutter-image-new)))
    (clutter-actor-set-size content 720.0 720.0)
    (clutter-image-set-data image
                            (gdk-pixbuf-get-pixels    pixbuf)
                            (if (gdk-pixbuf-get-has-alpha pixbuf)
                                :rgba-8888
                                :rgb-888)
                            (gdk-pixbuf-get-width     pixbuf)
                            (gdk-pixbuf-get-height    pixbuf)
                            (gdk-pixbuf-get-rowstride pixbuf)
                            (make-pointer 0))
    (g-object-unref pixbuf)
    (clutter-actor-set-content-scaling-filters
     content
     (const "CLUTTER_SCALING_FILTER_TRILINEAR")
     (const "CLUTTER_SCALING_FILTER_LINEAR"))
    (clutter-actor-set-content-gravity
     content
     (const "CLUTTER_CONTENT_GRAVITY_RESIZE_ASPECT"))
    (clutter-actor-set-content content image)
    (g-object-unref image)
    content))

(defcallback on-pan gboolean ((action (:pointer clutter-action))
                              (scroll (:pointer clutter-actor))
                              (is-interpolated gboolean)
                              (user-data :pointer))
  (cffi:with-foreign-objects ((delta-x :float) (delta-y :float))
    (c-let ((event clutter-event :ptr (make-pointer 0)))
      (cond
        ((eql 1 is-interpolated)
         (clutter-pan-action-get-interpolated-delta action delta-x delta-y))
        (t
         (clutter-gesture-action-get-motion-delta action 0 delta-x delta-y)
         (setf event (clutter-gesture-action-get-last-event action 0))))
      (format t "[~A] panning dx:~S dy:~S~%"
              (cond ((null-pointer-p (ptr event))
                     "INTERPOLATED")
                    ((= (c-ref event clutter-event :type) (const "CLUTTER_MOTION"))
                     "MOTION")
                    ((= (c-ref event clutter-event :type) (const "CLUTTER_TOUCH_UPDATE"))
                     "TOUCH UPDATE")
                    (t
                     "?"))
              (cffi:mem-ref delta-x :float)
              (cffi:mem-ref delta-y :float)))
    (const "TRUE")))

(defcallback clutter-main-quit :void ((actor (:pointer clutter-actor))
                                      (data :pointer))
  (clutter-main-quit))

(defun create-scroll-actor (stage)
  (let ((scroll (clutter-actor-new))
        (pan-action (clutter-pan-action-new)))
    (clutter-actor-set-name scroll "scroll")
    (clutter-actor-add-constraint scroll (clutter-align-constraint-new stage (const "CLUTTER_ALIGN_X_AXIS") 0.0))
    (clutter-actor-add-constraint scroll (clutter-bind-constraint-new stage (const "CLUTTER_BIND_SIZE") 0.0))
    (clutter-actor-add-child scroll (create-content-actor))
    (clutter-pan-action-set-interpolate pan-action (const "TRUE"))
    (g-signal-connect pan-action "pan" (callback 'on-pan))
    (clutter-actor-add-action scroll pan-action)
    (clutter-actor-set-reactive scroll (const "TRUE"))
    scroll))

(defcallback on-key-press gboolean ((stage (:pointer clutter-actor))
                                    (event (:pointer clutter-event))
                                    (unused :pointer))
  (let ((scroll (clutter-actor-get-first-child stage))
        (keysym (clutter-event-get-key-symbol event)))
    (when (= keysym (const "CLUTTER_KEY_space"))
      (clutter-actor-save-easing-state scroll)
      (clutter-actor-set-easing-duration scroll 1000)
      (clutter-actor-set-child-transform scroll (make-pointer 0))
      (clutter-actor-restore-easing-state scroll))
    (const "CLUTTER_EVENT_STOP")))

(defun pan-action ()
  (with-colors ((stage-color #x00 #x2b #x36)
                (actor-color #xff #xff #xff #x99))
    (with-foreign-objects ((argc :int)
                           (argv :pointer))
      (setf argc 0
            argv 0)
      (unless (= (const "CLUTTER_INIT_SUCCESS")
                 (clutter-init (make-pointer argc) (make-pointer argv)))
        (return-from pan-action)))
    (let ((stage (clutter-stage-new)))
      (clutter-stage-set-title stage "Pan Action")
      (clutter-stage-set-user-resizable stage (const "TRUE"))
      (let ((scroll (create-scroll-actor stage))
            (info   (clutter-text-new-with-text (ptr nil) "Press <space> to reset the image position.")))
        (clutter-actor-add-child stage scroll)
        (clutter-actor-add-child stage info)
        (clutter-actor-set-position info 12.0 12.0)
        (g-signal-connect stage "destroy" (callback 'clutter-main-quit))
        (g-signal-connect stage "key-press-event" (callback 'on-key-press))
        (clutter-actor-show stage))
      (main-with-cleanup stage))))
