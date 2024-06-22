(in-package :ml-survey/views)

(defun survey (survey &optional results)
  "Generates the view to show the survey created."
  (check-type survey ml-survey:survey)
  (with-page (:title "Survey Details")
    (body-header "Survey Details" (navbar-en))
    (:main :id "main-content"
	       :class "container"
	       (:p (format nil "ID: ~a" (ml-survey:survey-id survey)))
	       (:h2 "Properties")
	       (ml-survey:survey-html survey)
	       (when results
             (:h2 "Questionnaire Results")
	         (let ((count-answers (length (car results))))
               (:table :class "table table-hover"
                 (:caption "Questionnaire results table")
		         (:thead
		          (:tr
		           (loop for i from 1 to count-answers do
 		             (if (= i count-answers)
			             (:th :scope "col" "SUS Score")
			             (:th :scope "col" (format nil "Q ~a" i)))))
		          (:tbody
		           (loop for result in results do
		             (:tr
		              (loop for answer in result do
			            (:td answer))))))))))))
