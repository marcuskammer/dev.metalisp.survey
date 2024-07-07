(in-package :ml-survey/views)

(defun results-not-null (results)
  (some (lambda (x) (and (listp x) (not (null x)))) results))

(defun survey (survey &optional results)
  "Generates the view to show the survey created."
  (check-type survey ml-survey:survey)
  (let ((results-not-null (results-not-null results))
        (sus-results (getf results :sus)))

    (with-page (:title "Survey Details" :add-js-urls ("/app.js"))
      (body-header "Survey Details" (navbar-en))
      (:main :id "main-content"
	         :class "container"
	         (:p (format nil "ID: ~a" (ml-survey:survey-id survey)))
	         (:h2 :class "py-3" "Properties")
	         (ml-survey:survey-html survey)

	         (when results-not-null
               (:h2 :class "py-3" "Questionnaire Results")

               (if sus-results
                   (let ((count-answers (length (cdr (car sus-results)))))
                     (:h3 :class "py-1" "SUS")
                     (:table :class "table table-hover"
                       (:caption "Questionnaire results table")
		               (:thead
		                (:tr
	                     (:th :scope "col" "Time")
	                     (loop for header from 1 below count-answers
                               do (:th :scope "col" (format nil "Q ~a" header)))
                         (:th :scope "col" "SUS Score")))
		               (:tbody
		                (loop for row in sus-results
                              do (:tr (mapcar (lambda (data) (:td data)) row)))))))

               (loop for (type data) on results by #'cddr unless (eq type :sus)
                     do (progn (:h3 :class "py-1" (format nil "~a" type))
                               (loop for row in data
                                     do (:ul :class "list-group py-3"
                                             (loop for data in row
                                                   for i from 0
                                                   do (:li :class (if (zerop i)
                                                                      "list-group-item active"
                                                                      "list-group-item")
                                                           data)))))))))))
