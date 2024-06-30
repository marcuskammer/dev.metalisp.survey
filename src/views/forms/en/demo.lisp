(in-package :ml-survey/views)

(multi-form
  (:ask "How old are you?"
   :group "demo-1"
   :style "list-style:none;"
   :choices (:single "under 18"
                     "18 to 25"
                     "26 to 35"
                     "36 to 45"
                     "over 45")))
