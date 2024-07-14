;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/new-survey
  (:use :cl)
  (:import-from #:hunchentoot
                #:define-easy-handler)
  (:import-from #:dev.metalisp.sbt
                #:*use-cdn*
                #:with-page
                #:body-header))

(in-package :ml-survey/new-survey)

(setf *use-cdn* nil)

(defun list-questionnaires ()
  (mapcar #'ml-survey/fileops:extract-lang-and-filename
          (ml-survey/fileops:questionnaires-list-files)))

(defun view (&optional survey-id)
  "Generates the view to create a new survey."
  (let ((questionnaires (list-questionnaires)))
    (with-page (:title "New Survey")
      (body-header "New Survey" (ml-survey/navbar:navbar-en))
      (:main :class "container"
             :id "main-content"

             ;; If `questionnaires' is an empty list, show the user an warning
             ;; message.
             (unless questionnaires
               (:div :class "alert alert-warning"
                     :role "alert"
                     (format nil "There are no questionnaires available.~%
                                  The folder: ~a is empty." (ml-survey/fileops:questionnaires-dir))))

             ;; When a new survey was created, show the user an info message.
             (when survey-id
               (:div :class "alert alert-info"
                     :role "alert"
                     (format nil "Your new survey: ~A is created." survey-id)))

             (:form :action "/new-survey"
                    :method "post"

                    (:fieldset
                     (:legend "Metadata")
                     (:div :class "mb-3"
                           (:label :class "form-label"
                                   :for "title" "Title")
                           (:input :class "form-control"
                                   :type "text"
                                   :id "title"
                                   :required ""
                                   :name "title"))
                     (:div :class "mb-3"
                           (:label :class "form-label"
                                   :for "description" "Description")
                           (:textarea :class "form-control"
                                      :rows "3"
                                      :id "description"
                                      :name "description")))

                    (when questionnaires
                      (:fieldset
                       (:legend "Questionnaires")
                       (:div :class "mb-3"
                             (loop for q in questionnaires
                                   do (:div :class "form-check"
                                            (:input :class "form-check-input"
                                                    :type "checkbox"
                                                    :value q
                                                    :id q
                                                    :name "questionnaire"
                                                    (:label :class "form-check-label"
                                                            :for q
                                                            q)))))))

                    (:button :type"Submit"
                             :class "btn btn-primary"
                             "Create Survey"))))))

(defun process-new-survey-get ()
  (view))

(defun process-new-survey-post (request)
  (let ((post-params (hunchentoot:post-parameters* request))
        (uid (ml-survey/app:generate-uuid))
        (stored-surveys (ml-survey/fileops:read-from-file (ml-survey/fileops:make-surveys-db-file))))
    (ml-survey/fileops:write-to-file (ml-survey/fileops:make-surveys-db-file)
                                     (push (list uid post-params) stored-surveys))
    (view uid)))

(define-easy-handler (new-survey-handler :uri "/new-survey") nil
  (cond ((eq (hunchentoot:request-method*) :get)
         (process-new-survey-get))
        ((eq (hunchentoot:request-method*) :post)
         (process-new-survey-post hunchentoot:*request*))))
