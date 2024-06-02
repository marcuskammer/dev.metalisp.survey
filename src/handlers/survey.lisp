(in-package :ml-survey/handlers)

(defun survey-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 2)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts)))))

(defun survey-uri (request)
  (let ((uri (request-uri request)))
    (survey-uri-p uri)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let* ((id (subseq (request-uri*) (length "/survey/")))
         (survey (assoc (parse-integer id)
                        (load-response (make-surveys-db-path)))))
    (ml-survey/views:survey survey)))
