(in-package :ml-survey/handlers)

(define-easy-handler (surveys :uri "/") nil
  (let ((stored-surveys (load-response (make-surveys-db-path))))
    (ml-survey/views:surveys stored-surveys)))
