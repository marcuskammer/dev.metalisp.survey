(in-package :ml-survey/views)

(defun new-survey (&optional survey-id)
  "Generates the view to create a new survey."
  (let ((questionnaires (ml-survey:list-questionnaires)))
    (with-page (:title "New Survey")
      (body-header "New Survey" (navbar-en))
      (:main :class "container"
             :id "main-content"

             ;; If `questionnaires' is an empty list, show the user an warning
             ;; message.
             (unless questionnaires
               (:div :class "alert alert-warning"
                     :role "alert"
                     (format nil "There are no questionnaires available.~%
                                  The folder: ~a is empty." (ml-survey:forms-dir))))

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
