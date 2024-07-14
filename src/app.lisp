;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/app
  (:use #:cl)
  (:export #:extract-from
           #:*html-lang*
           #:generate-uuid
           #:split-uri
           #:today+now
           #:*app*
           #:start
           #:main))

(in-package #:ml-survey/app)

(defparameter *html-lang* "en")

(defparameter *use-cdn* nil)

(defparameter *url-key-map*
  '((:survey-id . 1)
    (:lang . 2)
    (:questionnaire . 3)))

(defun split-uri (uri)
  (check-type uri string)
  (remove-if #'string-empty-p
             (uiop:split-string uri :separator "/")))

(defun extract-from (url key)
  (let* ((parts (split-uri url))
         (index (cdr (assoc key *url-key-map*))))
    (when (and parts index)
      (nth index parts))))

(defun today ()
  "Return today's date formatted as ISO-8601."
  (local-time:format-timestring nil
                                (local-time:now)
                                :format '(:year "-" (:month 2) "-" (:day 2))))

(defun now ()
  "Return current time formatted as ISO-8601."
  (local-time:format-timestring nil
                                (local-time:now)
                                :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun today+now ()
   (format nil "~a ~a" (today) (now)))

(defun generate-uuid ()
  (parse-integer (format nil "~A~A~A"
                         (sb-posix:getpid)
                         (get-universal-time)
                         (random 1000000))))

(defun generate-random-id (length)
  (let ((charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (coerce (loop repeat length
                  collect (char charset (random (length charset))))
            'string)))

(defun string-empty-p (string) (= (length string) 0))

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
                             (ml-survey/fileops:public-dir)
                             :access-log-destination
                             (ml-survey/fileops:access-log-file))
  "The web server.")

(defun start ()
  "Start here. Start the web server."
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
