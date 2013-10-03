(defpackage clutter-raw)
(in-package :clutter-raw)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi-sys:%load-foreign-library
   :libclutter "/usr/lib64/libgdk_pixbuf-2.0.so")
  (cffi-sys:%load-foreign-library
   :libclutter "/usr/lib64/libglib-2.0.so")
  (cffi-sys:%load-foreign-library
   :libclutter "/usr/lib64/libclutter-1.0.so.0"))

(autowrap:c-include (cl:namestring (asdf:system-relative-pathname (asdf:find-system :cffi-clutter)
                                                               "clutter-raw.h"))
                    :exclude-arch ("i686-pc-linux-gnu"
                                   "i686-pc-win32"
                                   "x86_64-pc-win64"
                                   "i686-apple-darwin9"
                                   "x86_64-apple-darwin9")
                    :exclude-sources ("/usr/include/(?:getopt|pthread|sched|stdio|time|unistd|xlocale).h"
                                      ;; Adding this kills gdk-pixbuf..
                                      ;; "/usr/include/(?!stdint.h|bits/types.h|sys/types.h|glib-2.0|clutter-1.0|cogl).*"
                                      "/usr/include/clutter-1.0/clutter/deprecated"
                                      "/usr/include/glib-2.0/glib/deprecated")
                    :exclude-definitions ("^signal$")
                    :sysincludes '("/usr/include/"
                                   "/usr/include/linux"
                                   "/usr/include/glib-2.0"
                                   "/usr/lib64/glib-2.0/include"
                                   "/usr/include/cairo"
                                   "/usr/include/cogl"
                                   "/usr/include/clutter-1.0"
                                   "/usr/include/pango-1.0"
                                   "/usr/include/atk-1.0"
                                   "/usr/include/gdk-pixbuf-2.0"
                                   "/usr/include/json-glib-1.0")
                    :exclude-constants (".*")
                    :constant-accessor const)

(cl:in-package :cffi-clutter)
(use-package :clutter-raw)
