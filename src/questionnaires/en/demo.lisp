(in-package :ml-survey/questionnaire)

(multi-form
  (:ask "What is your age range?"
   :group "demo-age-range"
   :style "list-style-type:none"
   :choices (:single "18-24" "25-34" "35-44" "45-54" "55+"))
  (:ask "What is your gender?"
   :group "demo-gender"
   :style "list-style-type:none"
   :choices (:single "Male" "Female" "Non-binary" "Prefer not to say" "Other" :text "Other"))
  (:ask "What is your profession?"
   :group "demo-profession"
   :style "list-style-type:none"
   :choices (:text "Profession"))
  (:ask "What is your educational background?"
   :group "demo-edu"
   :style "list-style-type:none"
   :choices (:text "Last Degree")))
