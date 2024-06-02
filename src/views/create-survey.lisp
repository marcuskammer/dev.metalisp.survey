(in-package :ml-survey/views)

(defun create-survey (survey-id)
  "Generates the view to show the survey created."
  (with-page (:title "Surveys")
    (navbar-en)
    (:section :class "container"
              (:h2 "Your Surveys")
              (if survey-id
                  (:div :class "alert alert-info" :role "alert"
                        (format nil "Your new survey: ~A is created." survey-id))
                  nil))))
