(cl:eval-when (:load-toplevel :execute)
    (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cffi-clutter
  :version "0"
  :description "Bindings for Clutter gui library"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:cffi :cl-autowrap :alexandria :cl-plus-c)
  :components ((:file "clutter-raw")
               (:file "package"     :depends-on ("clutter-raw"))
               (:file "callbacks"   :depends-on ("package"))
               (:file "resource"    :depends-on ("package"))
               (:file "wrappers"    :depends-on ("package" "resource"))
               (:file "g-type"      :depends-on ("package"))
               (:file "g-value"     :depends-on ("package" "g-type"))
               (:file "pool"        :depends-on ("package"))))
