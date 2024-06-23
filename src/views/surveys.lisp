(in-package :ml-survey/views)

(defun surveys-p (list)
  "Check if all elements in `lst` are instances of the class `survey`."
  (every (lambda (item) (typep item 'ml-survey:survey)) list))

(deftype surveys-list ()
  '(and list (satisfies surveys-p)))

(defun surveys (surveys)
  "Generates the view to show all surveys available.

SURVEYS: List of survey objects."
  (check-type surveys surveys-list)
  (with-page (:title "Surveys")
    (body-header "Surveys" (navbar-en))
    (:main :id "main-content"
           :class "container"
           (:div :class "btn-toolbar my-3"
                 (:div :class "btn-group me-2"
                       (:a :class "btn btn-primary"
                           :href "/new-survey"
                           "New Survey")))
           (when surveys
             (:h2 :class "mb-3" "Overview")
             (:ol :class "list-group list-group-numbered"
                  (loop for survey in surveys
                        for title = (ml-survey:survey-properties-title survey)
                        for description = (ml-survey:survey-properties-description survey)
                        for id = (ml-survey:survey-id survey) do
                          (:li :class "list-group-item d-flex justify-content-between align-items-start"
                               (:div :class "ms-2 me-auto"
                                     (:a :class "fw-bold clearfix"
                                         :href (format nil "/survey/~a" id)
                                         title)
                                     (if description
                                         (:span description)
                                         nil)))))))))
