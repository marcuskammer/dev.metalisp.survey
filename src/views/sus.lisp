(in-package :ml-survey/views)

(defun load-form (lang form-file-name)
  "Load a Lisp file containing form definitions."
  (let* ((relative-path (concatenate 'string "src/views/forms/" lang "/"))
         (full-path (uiop:merge-pathnames* relative-path (uiop:getcwd)))
         (form-path (uiop:merge-pathnames* form-file-name full-path)))
    (unless (probe-file form-path)
      (error "Form file ~A does not exist." form-path))
    (load form-path))
  nil)

(defun sus-form (survey-id)
  (with-page (:title "SUS Form")
    (:section :class "container my-5"
              (:h2 "Usability Feedback Form")
              (:p "Please fill out the following forms and press the submit button.")
              (:form :action (format nil "/survey/~a/questionnaire/sus" survey-id)
                     :method "post"
                     :class (dev.metalisp.sbt/utility:spacing :property "m"
                                                              :side "y"
                                                              :size 5)
                     ;; load the multi-form from disk
                     (load-form *html-lang* "sus.lisp")

                     (btn-primary (:type "submit")
                       (find-l10n "submit" *html-lang* *l10n*))))))
