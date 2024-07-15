;;;; -*- mode: common-lisp; coding: utf-8; -*-
;;;; System usability scale (SUS) questionnaire in german language.

(in-package :ml-survey/questionnaire)

(spinneret:with-html (:p "Bitte beurteilen Sie auf einer Skala von
1 (Stimme überhaupt nicht zu) bis 5 (Stimme voll und ganz zu),
inwieweit Sie den folgenden Aussagen in Bezug auf die Ihnen
vorliegende Website zustimmen. Vielen Dank!"))

(multi-form
  (:ask "Ich denke, dass ich dieses System häufig nutzen möchte."
   :group "likert-1"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich fand das System unnötig komplex."
   :group "likert-2"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich fand das System einfach zu bedienen."
   :group "likert-3"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich glaube, ich bräuchte die Unterstützung einer technischen Person, um dieses System nutzen zu können."
   :group "likert-4"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich fand, dass die verschiedenen Funktionen in diesem System gut integriert waren."
   :group "likert-5"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich fand, dass dieses System zu widersprüchlich war."
   :group "likert-6"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich könnte mir vorstellen, dass die meisten Menschen sehr schnell lernen würden, mit diesem System umzugehen."
   :group "likert-7"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich fand das System sehr umständlich zu bedienen."
   :group "likert-8"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich fühlte mich sehr sicher im Umgang mit dem System."
   :group "likert-9"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu"))

  (:ask "Ich musste eine Menge Dinge lernen, bevor ich mit diesem System loslegen konnte."
   :group "likert-10"
   :style "list-style:none;"
   :choices (:single "1 Stimme überhaupt nicht zu"
                     "2 Stimme nicht zu"
                     "3 Weder zustimmen noch ablehnen"
                     "4 Zustimmen"
                     "5 Stimme voll und ganz zu")))
