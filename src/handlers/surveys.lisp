(in-package :ml-survey/handlers)

(define-easy-handler (surveys :uri "/") nil
  (ml-survey/views:surveys (mapcar (lambda (x)
                                     (make-instance 'ml-survey:survey
                                                    :id (format nil
                                                                "~a"
                                                                (first x))))
                                   (load-response (make-surveys-db-file)))))
