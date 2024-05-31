(in-package :ml-survey/views)

;; CREATE TABLE surveys (
;;   id SERIAL PRIMARY KEY,
;;   unique_id UUID DEFAULT uuid_generate_v4(),
;;   title VARCHAR(255),
;;   description TEXT,
;;   instructions TEXT,
;;   creator_id INTEGER,
;;   created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;   updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;; );

(defun new-survey ()
  "Generates the view to create a new survey."
  (with-page (:title "New Survey")
    (ml-survey/partials:navbar-en)
    (:section :class "container"
              (:h2 :class "mb-3" "New Survey")
              (:form :action "/create-survey" :method "post"

                     (:h3 "Metadata")
                     (:div :class "mb-3"
                           (:label :class "form-label"
                                   :for "title" "Title")
                           (:input :class "form-control"
                                   :type "text"
                                   :id "title"
                                   :name "title"))
                     (:div :class "mb-3"
                           (:label :class "form-label"
                                   :for "description" "Description")
                           (:textarea :class "form-control"
                                      :rows "3"
                                      :id "description"
                                      :name "description"))

                     (:h3 "Questionnaires")
                     (:div :class "mb-3"
                           (let ((questionnaires '(("System Usability Scale (Deutsch)" "/sus?lang=de")
                                                   ("System Usability Scale (English)" "/sus?lang=en"))))
                             (loop for el in questionnaires
                                   for name = (first el)
                                   for uri = (second el) do
                                     (:div :class "form-check"
                                           (:input :class "form-check-input"
                                                   :type "checkbox"
                                                   :value uri
                                                   :id uri
                                                   :name "questionnaire"
                                                   (:label :class "form-check-label"
                                                           :for uri
                                                           name))))))

                     (:button :type"Submit"
                              :class "btn btn-primary"
                              "Create Survey")))))
