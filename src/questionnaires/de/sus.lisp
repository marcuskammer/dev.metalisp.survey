;;;; -*- mode: common-lisp; coding: utf-8; -*-
;;;; System Usability Scale in german language.

(in-package :ml-survey/questionnaire)

(multi-form
  (:ask "Ich denke, dass ich dieses System häufig nutzen möchte."
   :group "likert-1"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich fand das System unnötig komplex."
   :group "likert-2"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich fand das System einfach zu bedienen."
   :group "likert-3"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich glaube, ich bräuchte die Unterstützung einer technischen Person, um dieses System nutzen zu können."
   :group "likert-4"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich fand, dass die verschiedenen Funktionen in diesem System gut integriert waren."
   :group "likert-5"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich fand, dass dieses System zu widersprüchlich war."
   :group "likert-6"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich könnte mir vorstellen, dass die meisten Menschen sehr schnell lernen würden, mit diesem System umzugehen."
   :group "likert-7"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich fand das System sehr umständlich zu bedienen."
   :group "likert-8"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich fühlte mich sehr sicher im Umgang mit dem System."
   :group "likert-9"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Ich musste eine Menge Dinge lernen, bevor ich mit diesem System loslegen konnte."
   :group "likert-10"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree")))
