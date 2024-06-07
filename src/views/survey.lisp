(in-package :ml-survey/views)

(defun survey (survey &optional results)
  "Generates the view to show the survey created."
  (with-page (:title "Survey Details")
    (navbar-en)
    (:section :class "container"
              (:h2 (format nil "Survey ID: ~a" (ml-survey/handlers::survey-id survey)))
              (:h3 "Properties")
              (ml-survey/handlers::survey-html survey)
              (:h3 "Questionnaire Results")
              (:ul
               (loop for result in results do
                 (:li result))))))
