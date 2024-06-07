(in-package :ml-survey/handlers)

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts)))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (request-uri request)))

(defun choose-sus-form (lang)
  "Based on LANG decide which sus form to show."
  (check-type lang string)
  (cond ((string= lang "en") #'ml-survey/views:sus-form-en)
        ((string= lang "de") #'ml-survey/views:sus-form-de)
        (t (error "Unsupported language: ~A" lang))))

(define-easy-handler (questionnaire :uri #'questionnaire-uri) (lang)
  (let ((s (make-instance 'survey :id (second (split-uri (request-uri*))))))
    (setf *html-lang* lang)
    (funcall (choose-sus-form lang) (survey-id s))))
