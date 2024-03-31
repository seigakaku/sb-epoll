;; Documentation strings come from man epoll(7)

("sys/epoll.h" "errno.h")

(;; epoll create flags
 (:integer epoll-cloexec "EPOLL_CLOEXEC" #D"Set the FD_CLOEXEC flag on the new file descriptor")

 ;; epoll_ctl op
 (:integer epoll-ctl-add "EPOLL_CTL_ADD"
           #D"Add an entry to the interest list of the epoll file descriptor, epfd.
              The entry includes the file descriptor, fd, a reference to the corresponding open file
              description (see epoll(7) and open(2)), and the settings specified in event.")
 (:integer epoll-ctl-mod "EPOLL_CTL_MOD"
           #D"Change the settings associated with fd in the interest list to the new settings specified in event.")

 (:integer epoll-ctl-del "EPOLL_CTL_DEL"
           #D"Remove (deregister) the target file descriptor fd from the interest list.
              The event argument is ignored and can be NULL (but see BUGS below).")

 ;; epoll_ctl events bitmask
 (:integer epollin "EPOLLIN" #D"The associated file is available for read(2) operations.")
 (:integer epollout "EPOLLOUT" #D"The associated file is available for write(2) operations.")

 (:integer epollrdhup "EPOLLRDHUP"
           #D"(since Linux 2.6.17)
              Stream socket peer closed connection, or shut down writing half of connection.
              (This flag is especially useful for writing simple code to detect peer shutdown when using edge-triggered monitoring.)")
 (:integer epollpri "EPOLLPRI"
           #D"There is an exceptional condition on the file descriptor.
              See the discussion of POLLPRI in poll(2).")
 (:integer epollerr "EPOLLERR"
           #D"Error condition happened on the associated file descriptor.
              This event is also reported for the write end of a pipe when the read end has been closed.

              epoll_wait(2) will always report for this event; it is not necessary to set it in events when calling epoll_ctl().")
 (:integer epollhup "EPOLLHUP"
           #D"Hang up happened on the associated file descriptor.

              epoll_wait(2) will always wait for this event; it is not necessary to set it in events when calling epoll_ctl().

              Note that when reading from a channel such as a pipe or a stream socket, this event merely indicates that the peer
              closed its end of the channel. Subsequent reads from the channel will return 0 (end of file) only after all out‐standing
              data in the channel has been consumed.")
 ;; epoll_ctl input flags
 (:integer epollet "EPOLLET"
           #D"Requests edge-triggered notification for the associated file descriptor.
              The default behavior for epoll is level-triggered.
              See epoll(7) for more detailed information about edge-triggered and level-triggered notification.")
 (:integer epolloneshot "EPOLLONESHOT"
           #D"(since Linux 2.6.2)
              Requests one-shot notification for the associated file descriptor.
              This means that after an event notified for the file descriptor by epoll_wait(2),
              the file descriptor is disabled in the interest list and no other events will be reported by the epoll interface.
              The user must call epoll_ctl() with EPOLL_CTL_MOD to rearm the file descriptor with a new event mask.")
 (:integer epollwakeup "EPOLLWAKEUP"
           #D"(since Linux 3.5)
              If EPOLLONESHOT and EPOLLET are clear and the process has the CAP_BLOCK_SUSPEND capability, ensure that the system
              does not enter 'suspend' or 'hibernate' while this event is pending or being processed. The event is considered as
              being 'processed' from the time when it is returned by a call to epoll_wait(2) until the next call to epoll_wait(2)
              on the same epoll(7) file descriptor, the closure of that file descriptor, the removal of the event file descriptor
              with EPOLL_CTL_DEL, or the clearing of EPOLLWAKEUP for the event file descriptor with EPOLL_CTL_MOD. See also BUGS.")
 (:integer epollexclusive "EPOLLEXCLUSIVE"
           #D"(since Linux 4.5)
              Sets an exclusive wakeup mode for the epoll file descriptor that is being attached to the target file descriptor, fd.
              When a wakeup event occurs and multiple epoll file descriptors are attached to the same target file using
              EPOLLEXCLU‐SIVE, one or more of the epoll file descriptors will receive an event with epoll_wait(2).
              The default in this sce‐nario (when EPOLLEXCLUSIVE is not set) is for all epoll file descriptors to receive an event.
              EPOLLEXCLUSIVE is thus useful for avoiding thundering herd problems in certain scenarios.

              If the same file descriptor is in multiple epoll instances, some with the EPOLLEXCLUSIVE flag, and others without,
              then events will be provided to all epoll instances that did not specify EPOLLEXCLUSIVE, and at least one of the
              epoll instances that did specify EPOLLEXCLUSIVE.

              The following values may be specified in conjunction with EPOLLEXCLUSIVE: EPOLLIN, EPOLLOUT, EPOLLWAKEUP, and EPOLLET.
              EPOLLHUP and EPOLLERR can also be specified, but this is not required: as usual, these events are always
              reported if they occur, regardless of whether they are specified in events.
              Attempts to specify other values in eventsyield the error EINVAL.

              EPOLLEXCLUSIVE may be used only in an EPOLL_CTL_ADD operation; attempts to employ it with EPOLL_CTL_MOD yield an error.
              If EPOLLEXCLUSIVE has been set using epoll_ctl(), then a subsequent EPOLL_CTL_MOD on the same epfd, fd pair yields an error.
              A call to epoll_ctl() that specifies EPOLLEXCLUSIVE in events and specifies the target
              file descriptor fd as an epoll instance will likewise fail. The error in all of these cases is EINVAL.")

 ;;;; Errors
 (:integer einval "EINVAL")
 (:integer emfile "EMFILE")
 (:integer enfile "ENFILE")
 (:integer enomem "ENOMEM")
 (:integer ebadf  "EBADF")
 (:integer eexist "EEXIST")
 (:integer eloop  "ELOOP")
 (:integer enoent "ENOENT")
 (:integer enospc "ENOSPC")
 (:integer eperm  "EPERM")
 (:integer efault "EFAULT")
 (:integer eintr  "EINTR"))
