(in-package :ml-survey/views)

(defun survey (id properties)
  "Generates the view to show the survey created."
  (check-type id string)
  (check-type properties list)
  (with-page (:title "Surveys")
    (navbar-en)
    (:section :class "container"
              (:h2 id)
              (:table :class "table"
                (:thead :class "thead-dark"
                        (:tr (:th :scope "col"
                                  "Key")
                             (:th :scope "col"
                                  "Value")))
                (:tbody (loop for property in properties
                              for key = (car property)
                              for value = (cdr property) do
                                (:tr (:td key)
                                     (:td (if (string= key "questionnaire")
                                              (:a :href (concatenate 'string "/survey/" id value)
                                                  value)
                                              value)))))))))
