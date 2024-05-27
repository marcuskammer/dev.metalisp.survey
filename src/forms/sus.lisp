(defun sus-form-en ()
  (with-page (:title "SUS Form" :main-con t)
    (navbar-en)
    (:h2 "Usability Feedback Form")
    (:p "Please fill out the following forms and press the submit button.")
    (:form :action "/submit"
           :method "post"
           :class (dev.metalisp.sbt/utility:spacing :property "m" :side "y" :size 5)
           (multi-form
             (:ask "I’d like to use this system frequently."
              :group "sus-1"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The system is unnecessarily complex."
              :group "sus-2"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The system is easy to use."
              :group "sus-3"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "I need the support of a technical person to use this system."
              :group "sus-4"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The functions in this system are well integrated."
              :group "sus-5"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "There is too much inconsistency in this system."
              :group "sus-6"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "Most people would learn to use this system very quickly."
              :group "sus-7"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The system is very awkward to use."
              :group "sus-8"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "I feel very confident using this system."
              :group "sus-9"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "I needed to learn a lot of things to get started with this system."
              :group "sus-10"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree")))

           (btn-primary (:type "submit")
             (find-l10n "submit" *html-lang* *l10n*)))))


(defun sus-form-de ()
  (with-page (:title "SUS Formular" :main-con t)
    (navbar-de)
    (:h2 "Usability Feedback Formular")
    (:p "Bitte füllen Sie die folgende Formular aus und klicken Sie auf die Schaltfläche 'Senden'.")
    (:form :action "/submit"
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
             (find-l10n "submit" *html-lang* *l10n*)))))
