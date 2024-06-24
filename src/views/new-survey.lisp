(in-package :ml-survey/views)

(defun new-survey ()
  "Generates the view to create a new survey."
  (with-page (:title "New Survey")
    (body-header "New Survey" (navbar-en))
    (:main :class "container"
           :id "main-content"

           (:form :action "/create-survey"
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

                  (:fieldset
                   (:legend "Questionnaires")
                   (:div :class "mb-3"
                         (let ((questionnaires '(("System Usability Scale (Deutsch)" "/sus?lang=de")
                                                 ("System Usability Scale (English)" "/sus?lang=en"))))
                           (loop for q in questionnaires
                                 for name = (first q)
                                 for uri = (second q)
                                 do (:div :class "form-check"
                                          (:input :class "form-check-input"
                                                  :type "checkbox"
                                                  :value uri
                                                  :id uri
                                                  :name "questionnaire"
                                                  (:label :class "form-check-label"
                                                          :for uri
                                                          name)))))))

                  (:button :type"Submit"
                           :class "btn btn-primary"
                           "Create Survey")))))
