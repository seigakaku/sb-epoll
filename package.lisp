(in-package #:common-lisp-user)

(defpackage #:sb-epoll
  (:use #:common-lisp #:sb-grovel #:sb-alien)
  (:export
   ;;;; Structs
   #:epoll_event #:epoll_data
   #:fd #:events #:data ; Slots
   ;;;; Create
   #:epoll-create #:epoll-create1
   #:epoll-cloexec ; Create flags
   ;;;; Control
   #:epoll-ctl
   #:epoll-ctl-add #:epoll-ctl-mod #:epoll-ctl-del ; Control operation
   #:epollin #:epollout #:epollrdhup #:epollpri #:epollerr #:epollhup ; Control events
   #:epollet #:epolloneshot #:epollwakeup #:epollexclusive ; Control flags
   ;;;; Wait
   #:epoll-wait #:epoll-pwait #:epoll-pwait2
   ;;;; Errors
   #:einval #:emfile #:enfile #:enomem #:ebadf #:eexist
   #:eloop #:enoent #:enospc #:eperm #:efault #:eintr))
