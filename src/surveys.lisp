;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/surveys
  (:use :cl)
  (:import-from #:hunchentoot
                #:define-easy-handler)
  (:import-from #:dev.metalisp.sbt
                #:body-header
                #:with-page))

(in-package :ml-survey/surveys)

(defun surveys-p (list)
  "Check if all elements in `lst` are instances of the class `survey`."
  (every (lambda (item) (typep item 'ml-survey/survey:survey)) list))

(deftype surveys-list ()
  '(and list (satisfies surveys-p)))

(defun view (surveys)
  "Generates the view to show all surveys available.

SURVEYS: List of survey objects."
  (check-type surveys surveys-list)
  (with-page (:title "Surveys" :add-js-urls ("/app.js"))
    (body-header "Surveys" (ml-survey/navbar:navbar-en))
    (:main :id "main-content"
           :class "container"
           (:div :class "btn-toolbar my-3"
                 (:div :class "btn-group me-2"
                       (:a :class "btn btn-primary"
                           :href "/new-survey"
                           "New Survey")))
           (when surveys
             (:h2 :class "mb-3" "Overview")
             (:ol :class "list-group list-group-numbered"
                  (loop for survey in surveys
                        for title = (ml-survey/survey:survey-properties-title survey)
                        for description = (ml-survey/survey:survey-properties-description survey)
                        for id = (ml-survey/survey:survey-id survey) do
                          (:li :class "list-group-item d-flex justify-content-between align-items-start"
                               (:div :class "ms-2 me-auto"
                                     (:a :class "fw-bold clearfix"
                                         :href (format nil "/survey/~a" id)
                                         title)
                                     (if description
                                         (:span description)
                                         nil)))))))))

(define-easy-handler (surveys-handler :uri "/") nil
  (let* ((survey-db (ml-survey/fileops:read-from-file (ml-survey/fileops:make-surveys-db-file)))
         (list-of-surveys (mapcar (lambda (x)
                                    (make-instance 'ml-survey/survey:survey
                                                   :id (format nil "~a" (first x))))
                                  survey-db)))
    (view list-of-surveys)))
