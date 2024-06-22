(in-package :ml-survey/views)

(defun load-form (lang form-file-name)
  "Load a Lisp file containing form definitions."
  (check-type lang string)
  (check-type form-file-name string)
  (let* ((relative-path (concatenate 'string "src/views/forms/" lang "/"))
         (full-path (uiop:merge-pathnames* relative-path (uiop:getcwd)))
         (form-path (uiop:merge-pathnames* form-file-name full-path)))
    (unless (probe-file form-path)
      (error "Form file ~A does not exist." form-path))
    (load form-path))
  nil)

(defmacro with-form (&body body)
  `(spinneret:with-html
    (:main :id "main-content"
           :class "container my-5"
           (:p "Please fill out the following forms and press the submit button.")
           (:form :action (request-uri*)
                  :method "post"
                  :class (spacing :property "m" :side "y" :size 5)
                  ,@body
                  (btn-primary (:type "submit")
                    (find-l10n "submit" ml-survey:*html-lang* *l10n*))))))

(defun sus-form ()
  (with-page (:title "SUS Form")
    (body-header "System Usability Form")
    (with-form (load-form ml-survey:*html-lang* "sus.lisp"))))
