;; Documentation strings come from man epoll(7)

("sys/epoll.h" "errno.h")

(;; epoll create flags
 (:integer epoll-cloexec "EPOLL_CLOEXEC")

 ;; epoll_ctl op
 (:integer epoll-ctl-add "EPOLL_CTL_ADD")
 (:integer epoll-ctl-mod "EPOLL_CTL_MOD")

 (:integer epoll-ctl-del "EPOLL_CTL_DEL")

 ;; epoll_ctl events bitmask
 (:integer epollin "EPOLLIN")
 (:integer epollout "EPOLLOUT")

 (:integer epollrdhup "EPOLLRDHUP")
 (:integer epollpri "EPOLLPRI")
 (:integer epollerr "EPOLLERR")
 (:integer epollhup "EPOLLHUP")
 ;; epoll_ctl input flags
 (:integer epollet "EPOLLET")
 (:integer epolloneshot "EPOLLONESHOT")
 (:integer epollwakeup "EPOLLWAKEUP")
 (:integer epollexclusive "EPOLLEXCLUSIVE")

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
