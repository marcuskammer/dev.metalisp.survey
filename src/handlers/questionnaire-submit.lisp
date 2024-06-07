(in-package :ml-survey/handlers)

(defun questionnaire-submit-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/submit'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (search "submit" (third parts)))))

(defun questionnaire-submit-uri (request)
  (questionnaire-submit-uri-p (request-uri request)))

(defun ensure-data-file-exist (survey-id questionnaire-id)
  (ensure-directories-exist (format nil "~a/~a/~a.lisp"
                                    (surveys-data-dir)
                                    survey-id
                                    questionnaire-id)))

(define-easy-handler (questionnaire-submit :uri #'questionnaire-submit-uri) nil
  (let ((post-params (post-parameters* *request*))
        (questionnaire-id (generate-uuid))
        (s (make-instance 'survey :id (second (split-uri (request-uri*))))))
    (store-response (ensure-data-file-exist (survey-id s) questionnaire-id)
                    post-params)
    (ml-survey/views:questionnaire-submit)))
