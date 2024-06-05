(in-package :ml-survey/handlers)

(defun questionnaire-submit-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/submit'"
  (let ((parts (split-uri uri))
        (survey (make-survey uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (funcall survey 'id)
         (search "submit" (third parts)))))

(defun questionnaire-submit-uri (request)
  (questionnaire-submit-uri-p (request-uri request)))

(defun ensure-data-file-exist (survey-id)
  (ensure-directories-exist (format nil "~a~a/~a.lisp"
                                    *survey-data-dir*
                                    survey-id
                                    (generate-uuid))))

(define-easy-handler (questionnaire-submit :uri #'questionnaire-submit-uri) nil
  (let ((post-params (post-parameters* *request*))
        (survey (make-survey (request-uri*))))
    (store-response (ensure-data-file-exist (funcall survey 'id)) post-params)
    (ml-survey/views:questionnaire-submit)))
