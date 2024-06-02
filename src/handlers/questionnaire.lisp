(in-package :ml-survey/handlers)

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (let ((parts (split-uri uri))
        (survey (make-survey uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (funcall survey 'id-p))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (request-uri request)))

(defun return-sus-form (lang)
  "Based on LANG decide which sus form to show."
  (check-type lang string)
  (cond ((string= lang "en") #'ml-survey/views:sus-form-en)
        ((string= lang "de") #'ml-survey/views:sus-form-de)
        (t (error "Unsupported language: ~A" lang))))

(define-easy-handler (questionnaire :uri #'questionnaire-uri) (lang)
  (let ((survey (make-survey (request-uri*))))
    (setf *html-lang* lang)
    (funcall (return-sus-form lang) (funcall survey 'id))))