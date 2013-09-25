(cl:eval-when (:load-toplevel :execute)
    (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cffi-clutter
  :version "0"
  :description "Bindings for Clutter gui library"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:cffi :cl-autowrap :alexandria)
  :components ((:file "package")
               (:file "clutter-raw" :depends-on ("package"))
               (:file "pool" :depends-on ("package" "inline-wrappers"))
               (:file "g-values" :depends-on ("package" "inline-wrappers"))
               (:file "wrappers" :depends-on ("package" "callbacks" "inline-wrappers" "pool"))
               (:file "resource" :depends-on ("package"))
               (:file "callbacks" :depends-on ("package" "inline-wrappers" "resource"))
               (:file "gobject-subclass" :depends-on ("package" "resource" "wrappers" "inline-wrappers"))
               (:file "animations" :depends-on ("package" "wrappers" "callbacks" "g-values"))))
