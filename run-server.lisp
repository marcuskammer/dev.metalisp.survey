(when (probe-file (merge-pathnames "slime/start-swank.lisp" (user-homedir-pathname)))
  (load "~/slime/start-swank.lisp"))
(ql:quickload :dev.metalisp.survey)
(ml-survey:start)
