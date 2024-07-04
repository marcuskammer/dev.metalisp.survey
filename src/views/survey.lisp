(in-package :ml-survey/views)

(defun results-not-null (results)
  (some (lambda (x) (and (listp x) (not (null x)))) results))

(defun survey (survey &optional results)
  "Generates the view to show the survey created."
  (check-type survey ml-survey:survey)
  (print results)
  (let ((results-not-null (results-not-null results))
        (sus-results (getf results :sus)))
    (with-page (:title "Survey Details")
      (body-header "Survey Details" (navbar-en))
      (:main :id "main-content"
	         :class "container"
	         (:p (format nil "ID: ~a" (ml-survey:survey-id survey)))
	         (:h2 "Properties")
	         (ml-survey:survey-html survey)

	         (when results-not-null
               (:h2 "Questionnaire Results")

               (if sus-results
                   (let ((count-answers (length (cdr (car sus-results)))))
                     (:h3 "SUS")
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
                              do (:tr (mapcar (lambda (x) (:td x)) result)))))))

               (loop for (type data) on results by #'cddr unless (eq type :sus)
                     do (progn (:h3 (format nil "~a" type))
                               (loop for row in data
                                     do (:ul :class "list-group py-3"
                                             (loop for item in row
                                                   do (:li :class "list-group-item"
                                                           item)))))))))))
