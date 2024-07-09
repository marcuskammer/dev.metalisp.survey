;;;; -*- mode: common-lisp; coding: utf-8; -*-

(in-package :ml-survey/handlers)

(defun process-new-survey-get ()
  (ml-survey/views:new-survey))

(defun process-new-survey-post (request)
  (let ((post-params (post-parameters* request))
        (uid (generate-uuid))
        (stored-surveys (read-from-file (make-surveys-db-file))))
    (write-to-file (make-surveys-db-file) (push (list uid post-params) stored-surveys))
    (ml-survey/views:new-survey uid)))

(define-easy-handler (new-survey :uri "/new-survey") nil
  (cond ((eq (hunchentoot:request-method*) :get)
         (process-new-survey-get))
        ((eq (hunchentoot:request-method*) :post)
         (process-new-survey-post *request*))))
