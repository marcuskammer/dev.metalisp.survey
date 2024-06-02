(in-package :ml-survey/views)

(defun questionnaire-submit ()
  (with-page (:title "Confirmation")
    (navbar-en)
    (:section :class "container"
              (:h2 "Confirmation")
              (:div :class "alert alert-info" :role "alert"
                    (format nil "Thank you for filling out the questionnaire.")))))
