(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))

(defun quicklisp-setup ()
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init))))

(quicklisp-setup)

(ql:quickload :dev.metalisp.survey)

(defvar os (uiop:detect-os))
(let ((app-name (uiop:os-cond ((uiop:os-windows-p) "ml-survey.exe")
                              ((uiop:os-unix-p) "ml-survey"))))
  (sb-ext:save-lisp-and-die app-name :executable t :toplevel 'ml-survey/app:main))

(quit)
