(in-package :cffi-clutter)

(defun g-signal-connect (instance detailed-signal c-handler &key (data nil) (destroy-data nil) (flags 0))
  (clutter-raw:g-signal-connect-data instance
                                     detailed-signal
                                     c-handler
                                     (if data data (null-pointer))
                                     (if destroy-data destroy-data (null-pointer))
                                     flags))
