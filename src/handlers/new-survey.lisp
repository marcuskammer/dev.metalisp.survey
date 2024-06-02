(in-package :ml-survey/handlers)

(define-easy-handler (new-survey :uri "/new-survey") nil
  (ml-survey/views:new-survey))
