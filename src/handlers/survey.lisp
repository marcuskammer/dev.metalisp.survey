(in-package :ml-survey/handlers)

(defun survey-id-p (id)
  (check-type id integer)
  (let ((ids (mapcar #'car (load-response (make-surveys-db-path)))))
    (if (member id ids) t nil)))

(defun survey-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (check-type uri string)
  (let ((parts (split-uri uri)))
    (and (= (length parts) 2)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (valid-survey-id-p (parse-integer (second parts))))))

(defun survey-uri (request)
  (let ((uri (request-uri request)))
    (survey-uri-p uri)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let* ((id (second (split-uri (request-uri*))))
         (survey (assoc (parse-integer id)
                        (load-response (make-surveys-db-path)))))
    (ml-survey/views:survey survey)))
