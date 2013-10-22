(defpackage :cffi-clutter
  (:use :cl :cffi :alexandria :autowrap :plus-c)
  (:shadowing-import-from :autowrap
                          #:foreign-type-size #:foreign-pointer
                          #:defcallback #:callback
                          #:define-foreign-type)
  (:export
   #:g-signal-connect
   ;;
   #:event-get-coords
   ;;
   #:set-color
   #:get-color
   #:with-color
   #:with-colors
   ;;
   #:init-clutter
   #:main-with-cleanup
   #:with-clutter-lock
   ;;
   #:animation-mode
   ;;
   #:alpha-set-mode
   ;;
   #:make-behaviour-path-with-knots
   ;;
   #:actor-get-preferred-size
   #:actor-get-preferred-width
   #:actor-get-preferred-height
   #:actor-set-size
   #:actor-get-position
   #:actor-get-geometry
   ;;
   #:stage-get-actor-at-position
   ;;
   #:score-append
   ;;
   #:with-perspective
   #:set-perspective
   #:get-perspective
   #:get-stage-perspective
   #:set-stage-perspective
   ;; resource.lisp
   #:resource
   #:register-resource
   #:unregister-resource
   ;; g-value.lisp
   #:g-type-lookup
   #:g-type-known
   #:g-type-to-ffi-type
   #:define-g-type
   #:g-value
   #:zero-g-value
   #:initialize-g-value
   #:make-g-value
   #:free-g-value
   #:with-g-value
   ))
