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

(defun sus-form-en (survey-id)
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
                     (load-form "en" "sus.lisp")

                     (btn-primary (:type "submit")
                       (find-l10n "submit" *html-lang* *l10n*))))))

(defun sus-form-de (survey-id)
  (with-page (:title "SUS Formular")
    (navbar-de)
    (:section :class "container"
              (:h2 "Usability Feedback Formular")
              (:p "Bitte füllen Sie die folgende Formular aus und klicken Sie auf die Schaltfläche 'Senden'.")
              (:form :action (format nil "/survey/~a/questionnaire/sus" survey-id)
                     :method "post"
                     :class (dev.metalisp.sbt/utility:spacing :property "m" :side "y" :size 5)
                     (multi-form
                       (:ask "Ich denke, dass ich dieses System häufig nutzen möchte."
                        :group "sus-1"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich fand das System unnötig komplex."
                        :group "sus-2"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich fand das System einfach zu bedienen."
                        :group "sus-3"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich glaube, ich bräuchte die Unterstützung einer technischen Person, um dieses System nutzen zu können."
                        :group "sus-4"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich fand, dass die verschiedenen Funktionen in diesem System gut integriert waren."
                        :group "sus-5"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich fand, dass dieses System zu widersprüchlich war."
                        :group "sus-6"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich könnte mir vorstellen, dass die meisten Menschen sehr schnell lernen würden, mit diesem System umzugehen."
                        :group "sus-7"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich fand das System sehr umständlich zu bedienen."
                        :group "sus-8"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich fühlte mich sehr sicher im Umgang mit dem System."
                        :group "sus-9"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree"))

                       (:ask "Ich musste eine Menge Dinge lernen, bevor ich mit diesem System loslegen konnte."
                        :group "sus-10"
                        :style "list-style:none;"
                        :choices (:single "1 Strongly Disagree"
                                          "2 Disagree"
                                          "3 Neither Agree nor Disagree"
                                          "4 Agree"
                                          "5 Strongly Agree")))

                     (btn-primary (:type "submit")
                       (find-l10n "submit" *html-lang* *l10n*))))))
