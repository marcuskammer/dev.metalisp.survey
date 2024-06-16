(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))

(defun quicklisp-setup ()
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init))))

(quicklisp-setup)

(ql:quickload :dev.metalisp.survey)
(sb-ext:save-lisp-and-die #P"ml-survey.core")
(quit)
