(in-package :ml-survey/views)

(defun load-form (lang questionnaire)
  "Load a Lisp file containing form definitions."
  (check-type lang string)
  (check-type questionnaire string)
  (let* ((form-path (uiop:merge-pathnames* (format nil "~a/~a.lisp" lang questionnaire)
                                           (ml-survey:ensure-questionnaires-dir))))
    (unless (probe-file form-path)
      (error "Form file ~A does not exist." form-path))
    (load form-path))
  nil)

(defmacro with-form (&body body)
  "Create a standardized HTML form wrapped in a <main> tag with a pre-defined
class and structure, using the Spinneret library. The form is designed to be
used within a web application served by Hunchentoot, utilizing common layout
and localization practices. The macro automatically sets the form’s action to
the current request URI and expects certain functions and variables to be
available in its environment for full functionality."
  `(spinneret:with-html
    (:main :id "main-content"
           :class "container my-5"
           (:p "Please fill out the following forms and press the submit button.")
           ;; action is defined as hunchentoot:request-uri* function
           (:form :action (request-uri*)
                  :method "post"
                  :class (spacing :property "m" :side "y" :size 5)
                  ,@body
                  (btn-primary (:type "submit")
                    (find-l10n "submit" ml-survey:*html-lang* *l10n*))))))

(defun questionnaire (questionnaire)
  (with-page (:title "SUS Form" :add-js-urls ("/app.js"))
    (body-header "System Usability Form")
    (with-form (load-form ml-survey:*html-lang* questionnaire))))
