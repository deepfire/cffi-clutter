(cl:defpackage :clutter-raw
  (:use :autowrap))

(cl:in-package :clutter-raw)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi-sys:%load-foreign-library
   :libclutter "/usr/lib64/libclutter-1.0.so.0"))

(autowrap:c-include "clutter-raw.h"
                    :exclude-arch ("i686-pc-linux-gnu"
                                   "i686-pc-win32"
                                   "x86_64-pc-win64"
                                   "i686-apple-darwin9"
                                   "x86_64-apple-darwin9")
                    :exclude-sources ("/usr/local/lib/clang/3.3/include/(?!stddef.h)"
                                      "/usr/include/(?!stdint.h|bits/types.h|sys/types.h|glib-2.0|clutter-1.0|cogl).*"
                                      "/usr/include/glib-2.0/glib/deprecated")
                    :sysincludes '("/usr/include/"
                                   "/usr/include/glib-2.0"
                                   "/usr/lib64/glib-2.0/include"
                                   "/usr/include/cairo"
                                   "/usr/include/cogl"
                                   "/usr/include/clutter-1.0"
                                   "/usr/include/pango-1.0"
                                   "/usr/include/atk-1.0"
                                   "/usr/include/json-glib-1.0")
                    :exclude-constants (".*")
                    :constant-accessor clutter-constant)

