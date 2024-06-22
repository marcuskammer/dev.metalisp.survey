(in-package :ml-survey/views)

(defun questionnaire-submit ()
  (with-page (:title "Confirmation")
    (body-header "Confirmation")
    (:main :id "main-content"
           :class "container"
           (:div :class "alert alert-info"
                 :role "alert"
                  "Thank you for filling out the questionnaire."))))
