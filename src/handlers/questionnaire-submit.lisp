(in-package :ml-survey/handlers)

(defun questionnaire-submit-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/submit'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (and (survey-id-p (parse-integer (second parts)))
              (every #'digit-char-p (second parts)))
         (search "submit" (third parts)))))

(defun questionnaire-submit-uri (request)
  (questionnaire-submit-uri-p (request-uri request)))

(defun ensure-data-file-exist (id &optional lang)
  (ensure-directories-exist (format nil "~a~a/~a-~a.lisp"
                                    *survey-data-dir*
                                    id
                                    (generate-uuid)
                                    lang)))

(define-easy-handler (questionnaire-submit :uri #'questionnaire-submit-uri) nil
  (let ((post-params (post-parameters* *request*))
        (id (survey-id (request-uri*))))
    (store-response (ensure-data-file-exist id) post-params)
    (ml-survey/views:questionnaire-submit)))
