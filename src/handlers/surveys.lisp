;;;; -*- mode: common-lisp; coding: utf-8; -*-

(in-package :ml-survey/handlers)

(define-easy-handler (surveys :uri "/") nil
  (let* ((survey-db (read-from-file (make-surveys-db-file)))
         (list-of-surveys (mapcar (lambda (x)
                                    (make-instance 'ml-survey:survey
                                                   :id (format nil "~a" (first x))))
                                  survey-db)))
    (ml-survey/views:surveys list-of-surveys)))
