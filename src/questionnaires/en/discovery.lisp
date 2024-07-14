;;;; -*- mode: common-lisp; coding: utf-8; -*-
;;;; Discovery Questionnaire in english language.

;;;; Discovery surveys are ideal for discovering your value
;;;; proposition and the challenges, problems and gains of customers.
;;;; Discovery surveys are not suitable for determining
;;;; what people will actually do, only what they say they want to do.

(in-package :ml-survey/questionnaire)

(multi-form
  (:ask "When was the last time you [insert scenario here]?"
   :group "date"
   :style "list-style:none"
   :choices (:date "Date"))
  (:ask "Can you describe what happened and what effects it had?"
   :group "longtext"
   :style "list-style:none"
   :choices (:text "Please describe"))
  (:ask "What other options have you considered? Why?"
   :group "longtext"
   :style "list-style:none"
   :choices (:text "Please describe")))
