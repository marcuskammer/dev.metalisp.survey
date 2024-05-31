(in-package :ml-survey/pages)

(defun index ()
  (let ((collection '("English" ("/sus?lang=en" "System Usability Scale")
                      "Deutsch" ("/sus?lang=de" "System Usability Scale"
                                 "/demographics?lang=de" "Demografie"))))
    (with-page (:title "Survey")
      (ml-survey/partials:navbar-en)
      (:section :class "container"
                (:h2 :class "mb-3" "Surveys")
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

(defun imprint ()
  nil)

(defun extract-title (list)
  (cdr (assoc "title" (second list) :test #'string-equal)))

(defun extract-description (list)
  (cdr (assoc "description" (second list) :test #'string-equal)))

(defun extract-id (list)
  (first list))

(defun surveys (surveys)
  "Generates the view to show all surveys available."
  (with-page (:title "Surveys")
    (ml-survey/partials:navbar-en)
    (:section :class "container"
              (:h2 :class "mb-3" "Your Surveys")
              (:div :class "btn-toolbar my-3"
                    (:div :class "btn-group me-2"
                          (:a :class "btn btn-primary"
                              :href "/new-survey"
                              "New Survey")))
              (:ol :class "list-group list-group-numbered"
                   (loop for survey in surveys
                         for title = (extract-title survey)
                         for description = (extract-description survey)
                         for id = (extract-id survey) do
                           (:li :class "list-group-item d-flex justify-content-between align-items-start"
                                (:div :class "ms-2 me-auto"
                                      (:a :class "fw-bold clearfix"
                                          :href (format nil "/survey/~A" id)
                                          title)
                                      (if description
                                          (:span description)
                                          nil))))))))

(defun create-survey (survey-id)
  "Generates the view to show the survey created."
  (with-page (:title "Surveys")
    (ml-survey/partials:navbar-en)
    (:section :class "container"
              (:h2 "Your Surveys")
              (if survey-id
                  (:div :class "alert alert-info" :role "alert"
                        (format nil "Your new survey: ~A is created." survey-id))
                  nil))))

(defun survey (survey)
  "Generates the view to show the survey created."
  (let ((id (format nil "~a" (first survey)))
        (properties (first (rest survey))))
    (with-page (:title "Surveys")
      (ml-survey/partials:navbar-en)
      (:section :class "container"
                (:h2 id)
                (:ul :class "list-group"
                     (loop for property in properties
                           for key = (car property)
                           for value = (cdr property) do
                             (:li :class "list-group-item"
                                  (if (string= key "questionnaire")
                                      (:a :href (concatenate 'string "/survey/" id value)
                                          value)
                                      (format nil "~a: ~a" key value)))))))))
