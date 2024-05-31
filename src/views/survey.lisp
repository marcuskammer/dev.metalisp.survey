(in-package :ml-survey/views)

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
