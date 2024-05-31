(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
(ql:quickload '(:dev.metalisp.survey))
(save-lisp-and-die "sbcl.core")
