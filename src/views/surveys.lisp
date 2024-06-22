(in-package :ml-survey/views)

(defun extract-title (list)
  (cdr (assoc "title" (second list) :test #'string-equal)))

(defun extract-description (list)
  (cdr (assoc "description" (second list) :test #'string-equal)))

(defun extract-id (list)
  (first list))

(defun surveys (surveys)
  "Generates the view to show all surveys available."
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
                        for title = (extract-title survey)
                        for description = (extract-description survey)
                        for id = (extract-id survey) do
                          (:li :class "list-group-item d-flex justify-content-between align-items-start"
                               (:div :class "ms-2 me-auto"
                                     (:a :class "fw-bold clearfix"
                                         :href (format nil "/survey/~a" id)
                                         title)
                                     (if description
                                         (:span description)
                                         nil)))))))))
