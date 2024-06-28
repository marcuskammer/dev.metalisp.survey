(in-package :ml-survey/handlers)

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/lang/type'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 4)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (= 2 (length (third parts))))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (request-uri request)))

(defun process-questionnaire-get (lang questionnaire)
  (check-type lang string)
  (setf ml-survey:*html-lang* lang)
  (ml-survey/views:questionnaire questionnaire))

(defun process-questionnaire-post (request survey)
  (let* ((post-params (post-parameters* request))
         (questionnaire-id (generate-uuid))
         (questionnaire-data-file (ensure-data-file-exist (ml-survey:survey-id survey)
                                                          questionnaire-id)))
    (store-response questionnaire-data-file (push (today+now) post-params))
    (ml-survey/views:questionnaire-submit)))

(define-easy-handler (questionnaire :uri #'questionnaire-uri) (lang)
  (let ((s (make-instance 'ml-survey:survey
                          :id (extract-from (request-uri*) :survey-id))))
    (cond ((eq (hunchentoot:request-method*) :get)
           (process-questionnaire-get (extract-from (request-uri*) :language)
                                      (extract-from (request-uri*) :questionnaire)))
          ((eq (hunchentoot:request-method*) :post)
           (process-questionnaire-post *request* s)))))
