export APP_HOME=~/quicklisp/local-projects/dev.metalisp.survey
sbcl --noinform --core sbcl.core-with-swank --eval '(ml-survey:start-server ml-survey:*app* :document-root (uiop:getenv "APP_HOME"))'
