(in-package :ml-survey/handlers)

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/questionnaire/type'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 4)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (string= (third parts) "questionnaire"))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (request-uri request)))

(defun choose-sus-form (lang)
  "Based on LANG decide which sus form to show."
  (check-type lang string)
  (case (string-downcase lang)
    ("en" #'ml-survey/views:sus-form-en)
    ("de" #'ml-survey/views:sus-form-de)
    (otherwise (error "Unsupported language: ~A" lang))))

(defun process-questionnaire-get (lang survey)
  (check-type lang string)
  (check-type survey ml-survey:survey)
  (setf *html-lang* lang)
  (funcall (choose-sus-form lang) (ml-survey:survey-id survey)))

(defun process-questionnaire-post (request survey)
  (let ((post-params (post-parameters* request))
        (questionnaire-id (generate-uuid)))
    (store-response (ensure-data-file-exist (ml-survey:survey-id survey)
                                            questionnaire-id)
                    post-params)
    (ml-survey/views:questionnaire-submit)))

(define-easy-handler (questionnaire :uri #'questionnaire-uri) (lang)
  (let ((s (make-instance 'ml-survey:survey :id (get-survey-id (request-uri*)))))
    (cond ((eq (hunchentoot:request-method*) :get)
           (process-questionnaire-get lang s))
          ((eq (hunchentoot:request-method*) :post)
           (process-questionnaire-post *request* s)))))
