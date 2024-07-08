;;;; -*- mode: common-lisp; coding: utf-8; -*-

(in-package :ml-survey)

(defparameter *html-lang* "en")

(defun set-default-directory (directory)
  (setf *default-pathname-defaults* (truename (merge-pathnames directory))))

(defun create-server (name port &key address document-root access-log-destination)
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address address
                                 :name name
                                 :document-root document-root
                                 :access-log-destination access-log-destination
                                 :port port)))
    acceptor))

(defun start-server (acceptor &key document-root)
  (if document-root
      (setf (hunchentoot:acceptor-document-root acceptor) document-root))
  (hunchentoot:start acceptor))

(defun stop-server (acceptor)
  (hunchentoot:stop acceptor))

(defun restart-server (acceptor)
  (hunchentoot:stop acceptor)
  (hunchentoot:start acceptor))

(defvar *app* (create-server 'app
                             8080
                             :document-root
                             (public-dir)
                             :access-log-destination
                             (access-log-file)))

(defun start ()
  (start-server *app*))

(defun main ()
  "Call this function automatically from binary lisp image. Out of a REPL use
`start' function."
  (start)
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  ;; You can simply run (sleep most-positive-fixnum)
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (format *error-output* "Aborting.~&")
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
