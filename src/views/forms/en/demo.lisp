(in-package :ml-survey/views)

(multi-form
  (:ask "What is your age range?"
   :group "demo-age-range"
   :choices (:single "18-24" "25-34" "35-44" "45-54" "55+"))
  (:ask "What is your gender?"
   :group "demo-gender"
   :choices (:single "Male" "Female" "Non-binary" "Prefer not to say" "Other" :text "Other"))
  (:ask "What is your profession?"
   :group "demo-profession"
   :choices (:text "Profession"))
  (:ask "What is your educational background?"
   :group "demo-edu"
   :choices (:text "Last Degree")))
