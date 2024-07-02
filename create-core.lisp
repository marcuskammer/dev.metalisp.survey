(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))

(defun quicklisp-setup ()
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init))))

(quicklisp-setup)

(ql:quickload :dev.metalisp.survey)
(let ((app-name (uiop:os-cond ((eq :os-windows (uiop:detect-os)) "survey-buttler.exe")
                              ((eq :os-unix (uiop:detect-os)) "survey-buttler"))))
  (sb-ext:save-lisp-and-die app-name :executable t :toplevel 'ml-survey:main))
(quit)
