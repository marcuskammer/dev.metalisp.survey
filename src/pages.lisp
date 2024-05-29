(in-package :dev.metalisp.survey/pages)

(defun index ()
  (let ((collection '("English" ("/sus?lang=en" "System Usability Scale")
                      "Deutsch" ("/sus?lang=de" "System Usability Scale"
                                 "/demographics?lang=de" "Demografie"))))
    (with-page (:title "Survey")
      (dev.metalisp.survey/partials:navbar-en)
      (:section :class "container"
                (:h2 :class "mb-3" "Surveys")
                (:div :class "btn-toolbar my-3"
                      (:div :class "btn-group me-2"
                            (:a :class "btn btn-primary"
                                :href "/new-survey"
                                "New Survey")))
                (loop for (lang anchors) on collection by #'cddr do
                  (:section :class "container-fluid mb-3"
                            (:h3 :class "mb-3" lang)
                            (:ul :class "list-group"
                                 (loop for (url name) on anchors by #'cddr do
                                   (:li :class "list-group-item"
                                        (:a :href url name))))))))))

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
    (dev.metalisp.survey/partials:navbar-en)
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
                           (let ((questionnaires '(("sus-de" "System Usability Scale (Deutsch)")
                                                   ("sus-en" "System Usability Scale (English)"))))
                             (loop for el in questionnaires do
                               (:div :class "form-check"
                                     (:input :class "form-check-input"
                                             :type "checkbox"
                                             :value "t"
                                             :id (first el)
                                             :name (first el)
                                             (:label :class "form-check-label"
                                                     :for (first el)
                                                     (second el)))))))

                     (:button :type"Submit"
                              :class "btn btn-primary"
                              "Create Survey")))))

(defun imprint ()
  nil)
