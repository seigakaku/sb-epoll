;;;; epoll.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(asdf:defsystem #:sb-epoll
  :description "SBCL sb-alien bindings for epoll(7)"
  :author "Iosevka <iosevka AT navert DOT com>"
  :serial t
  :defsystem-depends-on (#:sb-grovel)
  :components ((:file "package")
               (sb-grovel:grovel-constants-file "epoll-constants" :package #:sb-epoll)
               (:file "sb-epoll")))
