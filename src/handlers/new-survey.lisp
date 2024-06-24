(in-package :ml-survey/handlers)

(defun process-new-survey-get ())

(defun process-new-survey-post (request)
  (let ((post-params (post-parameters* *request*))
        (uid (generate-uuid))
        (stored-surveys (load-response (make-surveys-db-file))))
    (store-response (make-surveys-db-file) (push (list uid post-params) stored-surveys))))

(define-easy-handler (new-survey :uri "/new-survey") nil
  (ml-survey/views:new-survey))
