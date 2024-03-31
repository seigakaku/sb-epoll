;;;; epoll.lisp

(in-package #:sb-epoll)

(defmacro define-alien-routine* (name retval docstring &body params)
  `(prog1
       (define-alien-routine ,name ,retval
         ,@params)
     ,@(when docstring
         `((setf (documentation ',(sb-alien::pick-lisp-and-alien-names name) 'function)
                 ,docstring)))))

(defmacro define-alien-type* (name &body body)
  (let ((doc (when (stringp (car body))
               (car body))))
    `(prog1
         (define-alien-type ,name
             ,@(if doc (cdr body) body))
       ,@(when doc
           `((setf (documentation ',name 'type) ,doc))))))

(define-alien-type epoll_data
    (union epoll_data
           (ptr (* t))
           (fd int)
           (u32 (unsigned 32))
           (u64 (unsigned 64))))

(define-alien-type epoll_data_t epoll_data)

(define-alien-type* epoll_event
  "The `epoll_event' structure specifies data that the kernel should save and return
when the corresponding file descriptor becomes ready."
  (struct epoll_event
          (events (unsigned 32))
          (data epoll_data)))

(define-alien-routine* ("epoll_create" epoll-create) int
    "epoll_create() creates a new epoll(7) instance.
Since Linux 2.6.8, the size argument is ignored, but must be greater than zero."
  (size int))

(define-alien-routine* ("epoll_create1" epoll-create1) int
    "If flags is 0, then, other than the fact that the obsolete size argument is dropped,
epoll_create1() is the same as epoll_create().
The following value can be included in flags to obtain different behavior:

EPOLL_CLOEXEC
  Set  the  close-on-exec  (FD_CLOEXEC)  flag on the new file descriptor.
  See the description of the O_CLOEXEC flag in
  open(2) for reasons why this may be useful."
  (flags int))

(define-alien-routine* ("epoll_ctl" epoll-ctl) int
    "This system call is used to add, modify, or remove entries in the interest list of the epoll(7) instance
referred to by the file descriptor epfd.
It requests that the operation op be performed for the target file descriptor, fd."
  (epfd int)
  (op int)
  (fd int)
  (event (* (struct epoll-event))))

(define-alien-routine* ("epoll_wait" epoll-wait) int
    "The epoll_wait() system call waits for events on the epoll(7) instance referred to by the file descriptor epfd.
The buffer pointed to by events is used to return information from the ready list about file descriptors
in the interest list that have some events available.

Up to maxevents are returned by epoll_wait(). The maxevents argument must be greater than zero.

The timeout argument specifies the number of milliseconds that epoll_wait() will block.
Time is measured against the CLOCK_MONOTONIC clock.

A call to epoll_wait() will block until either:

• a file descriptor delivers an event;

• the call is interrupted by a signal handler; or

• the timeout expires.

Note that the timeout interval will be rounded up to the system clock granularity, and kernel scheduling delays mean that
the blocking interval may overrun by a small amount. Specifying a timeout of -1 causes epoll_wait() to block indefinitely,
while specifying a timeout equal to zero causes epoll_wait() to return immediately, even if no events are available.

The struct epoll_event is described in epoll_event(3type).

The data field of each returned epoll_event structure contains the same data as was specified in the most recent call to
epoll_ctl(2) (EPOLL_CTL_ADD, EPOLL_CTL_MOD) for the corresponding open file descriptor.

The events field is a bit mask that indicates the events that have occurred for the corresponding open file description.
See epoll_ctl(2) for a list of the bits that may appear in this mask."
  (epfd int)
  (events (* (struct epoll-event)))
  (maxevents int)
  (timeout int))

;;;; FIXME: Add the sigset struct?
(define-alien-routine* ("epoll_pwait" epoll-pwait) int
    "The  relationship  between epoll_wait() and epoll_pwait() is analogous to the relationship between select(2) and pselect(2):
like pselect(2), epoll_pwait() allows an application to safely wait until either a file descriptor becomes ready or until  a
signal is caught.

The following epoll_pwait() call:

    ready = epoll_pwait(epfd, &events, maxevents, timeout, &sigmask);

is equivalent to atomically executing the following calls:

    sigset_t origmask;

    pthread_sigmask(SIG_SETMASK, &sigmask, &origmask);
    ready = epoll_wait(epfd, &events, maxevents, timeout);
    pthread_sigmask(SIG_SETMASK, &origmask, NULL);

The sigmask argument may be specified as NULL, in which case epoll_pwait() is equivalent to epoll_wait()."
  (epfd int)
  (events (* (struct epoll-event)))
  (maxevents int)
  (timeout int)
  (sigmask (* (struct sigset)))) ;; nullable SIGSET*

(define-alien-routine* ("epoll_pwait2" epoll-pwait2) int
    "The `epoll_pwait2()' system call is equivalent to `epoll_pwait()' except for the timeout argument.
It takes an argument of type timespec to be able to specify nanosecond resolution timeout.
This argument functions the same as in `pselect(2)' and `ppoll(2)'.
If timeout is NULL, then `epoll_pwait2()' can block indefinitely."
  (epfd int)
  (events (* (struct epoll-event)))
  (maxevents int)
  (timeout (* (struct timespec)))
  (sigmask (* (struct sigset))))
