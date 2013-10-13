(cl:eval-when (:load-toplevel :execute)
    (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cffi-clutter
  :version "0"
  :description "Bindings for Clutter gui library"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:cffi :cl-autowrap :alexandria :cl-plus-c)
  :components ((:file "package")
               (:file "clutter-raw" :depends-on ("package"))
               (:file "callbacks"   :depends-on ("package" "clutter-raw"))
               (:file "resource"    :depends-on ("package" "clutter-raw"))
               (:file "wrappers"    :depends-on ("package" "clutter-raw" "resource"))
               (:file "pool"        :depends-on ("package" "clutter-raw"))))
