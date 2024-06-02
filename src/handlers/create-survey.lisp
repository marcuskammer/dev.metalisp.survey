(in-package :ml-survey/handlers)

(define-easy-handler (create-survey :uri "/create-survey") nil
  (let ((post-params (post-parameters* *request*))
        (uid (generate-uuid))
        (stored-surveys (load-response (make-surveys-db-path))))
    (store-response (make-surveys-db-path) (push (list uid post-params) stored-surveys))
    (ml-survey/views:create-survey uid)))
