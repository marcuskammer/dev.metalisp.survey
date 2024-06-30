(in-package :ml-survey/views)

(defun survey (survey &optional results)
  "Generates the view to show the survey created."
  (check-type survey ml-survey:survey)
  (let ((results-not-null (some (lambda (x) (and (listp x) (not (null x)))) results))
        (sus-results (getf results :sus))
        (other-results (getf results :other)))
    (with-page (:title "Survey Details")
      (body-header "Survey Details" (navbar-en))
      (:main :id "main-content"
	         :class "container"
	         (:p (format nil "ID: ~a" (ml-survey:survey-id survey)))
	         (:h2 "Properties")
	         (ml-survey:survey-html survey)
	         (when results-not-null
               (:h2 "Questionnaire Results")
               (when sus-results
                 (let ((count-answers (length (cdr (car sus-results)))))
                   (:table :class "table table-hover"
                     (:caption "Questionnaire results table")
		             (:thead
		              (:tr
	                   (:th :scope "col" "Time")
	                   (loop for i from 1 below count-answers
                             do (:th :scope "col" (format nil "Q ~a" i)))
                       (:th :scope "col" "SUS Score")))
		             (:tbody
		              (loop for result in sus-results
                            do (:tr (mapcar (lambda (x) (:td x)) result))))))))))))
