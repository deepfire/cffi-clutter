(defpackage #:clutter-examples
  (:use #:cl #:iterate #:alexandria
        #:cffi #:autowrap #:plus-c
        #:clutter-raw #:cffi-clutter)
  (:shadowing-import-from :autowrap
                          #:foreign-type-size #:foreign-pointer
                          #:defcallback #:callback
                          #:define-foreign-type)
  (:export #:pan-action))
