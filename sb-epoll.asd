;;;; epoll.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defun %read-docstring (stream &optional (close-char #\")
                                 (whitespace '(#\Space #\Rubout #\Tab)))
  (coerce
   (loop
     with marginp = nil
     for ch = (read-char-no-hang stream t nil t)
     until (char= ch close-char)
     nconc (cond
             ((and marginp (member ch whitespace)) nil)
             ((char= ch #\Newline)
              (setf marginp t)
              (list #\\ #\n))
             (t (setf marginp nil)
                (list ch))))
   'string))

(defun read-docstring (stream ch arg)
  (declare (ignore ch arg))
  (read-char-no-hang stream t nil t)
  (%read-docstring stream))

(defun make-grovel-readtable ()
  "Make a readtable for the groveler, with #D read macro for reading docstrings formatted
like the lisp ones and spitting out something the groveller can use in the C code."
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\D #'read-docstring rt)
    rt))

;;;; Temporarily rebind the internal groveller function `C-CONSTANTS-EXTRACT'
;;;; which is where the constants file is opened and read; to a function
;;;; that binds a readtable returned by `MAKE-GROVEL-READTABLE'
(defmethod perform :around ((op compile-op) (c sb-grovel:grovel-constants-file))
  (let ((original-fn (fdefinition 'sb-grovel::c-constants-extract)))
    (unwind-protect
         (progn
           (setf (fdefinition 'sb-grovel::c-constants-extract)
                 (lambda (input output package)
                   (let ((*readtable* (make-grovel-readtable)))
                     (funcall original-fn input output package))))
           (when (next-method-p)
             (call-next-method)))
      (setf (fdefinition 'sb-grovel::c-constants-extract) original-fn))))

(asdf:defsystem #:sb-epoll
  :description "SBCL sb-alien bindings for epoll(7)"
  :author "Iosevka <iosevka AT navert DOT com>"
  :license  "LGPLv2"
  :serial t
  :defsystem-depends-on (#:sb-grovel)
  :components ((:file "package")
               (sb-grovel:grovel-constants-file "epoll-constants" :package #:sb-epoll)
               (:file "sb-epoll")))
