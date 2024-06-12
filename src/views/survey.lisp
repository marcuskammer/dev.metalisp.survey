(in-package :ml-survey/views)

(defun survey (survey &optional results)
  (check-type survey ml-survey:survey)
  "Generates the view to show the survey created."
  (with-page (:title "Survey Details")
    (navbar-en)
    (:section :class "container"
              (:h2 (format nil "Survey ID: ~a" (ml-survey:survey-id survey)))
              (:h3 "Properties")
              (ml-survey:survey-html survey)
              (when results
                (:h3 "Questionnaire Results")
                (:ul
                 (loop for result in results do
                   (:li result)))))))
