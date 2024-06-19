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

(defun sus-form ()
  (with-form (:title "SUS Form")
    (:main :id "main-content"
           :class "container my-5"
           (:h1 "Usability Feedback Form")
           (:p "Please fill out the following forms and press the submit button.")
           (:form :action (hunchentoot:request-uri*)
                  :method "post"
                  :class (spacing :property "m" :side "y" :size 5)
                  ;; load the multi-form from disk
                  (load-form spinneret:*html-lang* "sus.lisp")
                  (btn-primary (:type "submit")
                    (find-l10n "submit" spinneret:*html-lang* *l10n*))))))
