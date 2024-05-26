(in-package :dev.metalisp.survey)

(defmacro navbar-nav (&body body)
  `(spinneret:with-html
     (:div :class "collapse navbar-collapse"
           :id "#mainNav"
           (:ul :class "navbar-nav"
                ,@(loop for navitem on body by #'cddr
                        collect
                        `(:li :class "nav-item"
                              (:a :class "nav-link"
                                  :href ,(first navitem)
                                  ,(second navitem))))))))

(defmacro navbar-en ()
  `(spinneret:with-html
     (:nav :class "navbar bg-body-tertiary navbar-expand-sm mb-5"
           (:div :class "container-fluid"
                 ;; brand
                 (:a :class "navbar-brand"
                     :href "#"
                     (:img :src "/src/assets/company_logo.png"
                           :alt "Company Logo"
                           :width 100))
                 ;; toggle button
                 (:button :class "navbar-toggler"
                          :type "button"
                          :data-bs-toggle "collapse"
                          :data-bs-target "#mainNav"
                          :aria-controls "#mainNav"
                          :aria-expanded "false"
                          :aria-label "Toggle Navigation"
                          (:span :class "navbar-toggler-icon"))
                 ;; nav
                 (navbar-nav "/" "Home" "/imprint" "Imprint")))))

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
