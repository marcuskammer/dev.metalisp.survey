(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))

(defun quicklisp-setup ()
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init))))

(quicklisp-setup)

(defun slime-setup ()
  (let ((slime-directory (merge-pathnames "common-lisp/slime/" (user-homedir-pathname)))
        (slime-git-url "https://github.com/slime/slime.git")
        (slime-tag "v2.30"))

    (unless (probe-file slime-directory)
      (ensure-directories-exist slime-directory)
      (uiop:run-program (format nil "git clone -b ~a ~a ~a" slime-tag slime-git-url slime-directory)))

    (let ((swankloader (merge-pathnames "swank-loader.lisp" slime-directory)))
      (when (probe-file swankloader)
        (load swankloader)))))

(slime-setup)

(ql:quickload :dev.metalisp.survey)
(swank-loader:dump-image "sbcl.core-with-swank")
