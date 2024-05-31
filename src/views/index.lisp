(in-package :ml-survey/views)

(defun index ()
  (let ((collection '("English" ("/sus?lang=en" "System Usability Scale")
                      "Deutsch" ("/sus?lang=de" "System Usability Scale"
                                 "/demographics?lang=de" "Demografie"))))
    (with-page (:title "Survey")
      (ml-survey/partials:navbar-en)
      (:section :class "container"
                (:h2 :class "mb-3" "Surveys")
                (loop for (lang anchors) on collection by #'cddr do
                  (:section :class "container-fluid mb-3"
                            (:h3 :class "mb-3" lang)
                            (:ul :class "list-group"
                                 (loop for (url name) on anchors by #'cddr do
                                   (:li :class "list-group-item"
                                        (:a :href url name))))))))))
