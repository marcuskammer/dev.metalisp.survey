;;;; -*- mode: common-lisp; coding: utf-8; -*-
;;;; System Usability Scale in english language.

(in-package :ml-survey/questionnaire)

(spinneret:with-html (:p "On a scale from 1 (strongly disagree)
to 5 (strongly agree), please rate the extent to which you agree
with the following statements about the website. Thank you very much!"))

(multi-form
  (:ask "I would like to use this system frequently."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "The system is unnecessarily complex."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "The system is easy to use."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "I need the support of a technical person to use this system."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "The functions in this system are well integrated."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "There is too much inconsistency in this system."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "Most people would learn to use this system very quickly."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "The system is very awkward to use."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "I feel very confident using this system."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree"))

  (:ask "I needed to learn a lot of things to get started with this system."
   :group "likert"
   :style "list-style:none;"
   :choices (:single "1 Strongly Disagree"
                     "2 Disagree"
                     "3 Neither Agree nor Disagree"
                     "4 Agree"
                     "5 Strongly Agree")))
