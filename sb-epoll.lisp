;;;; epoll.lisp

(in-package #:sb-epoll)

(define-alien-type epoll_data
    (union epoll_data
           (ptr (* t))
           (fd int)
           (u32 (unsigned 32))
           (u64 (unsigned 64))))

(define-alien-type epoll_data_t epoll_data)

(define-alien-type epoll_event
    (struct epoll_event
            (events (unsigned 32))
            (data epoll_data)))

(define-alien-routine ("epoll_create" epoll-create) int
  (size int))

(define-alien-routine ("epoll_create1" epoll-create1) int
  (flags int))

(define-alien-routine ("epoll_ctl" epoll-ctl) int
  (epfd int)
  (op int)
  (fd int)
  (event (* (struct epoll-event))))

(define-alien-routine ("epoll_wait" epoll-wait) int
  (epfd int)
  (events (* (struct epoll-event)))
  (maxevents int)
  (timeout int))

;;;; FIXME: Add the sigset struct?
(define-alien-routine ("epoll_pwait" epoll-pwait) int
  (epfd int)
  (events (* (struct epoll-event)))
  (maxevents int)
  (timeout int)
  (sigmask (* (struct sigset)))) ;; nullable SIGSET*

(define-alien-routine ("epoll_pwait2" epoll-pwait2) int
  (epfd int)
  (events (* (struct epoll-event)))
  (maxevents int)
  (timeout (* (struct timespec)))
  (sigmask (* (struct sigset))))
