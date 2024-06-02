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
         (survey-id-p (parse-integer (second parts))))))

(defun survey-uri (request)
  (let ((uri (request-uri request)))
    (survey-uri-p uri)))

(defun survey-id (uri)
  (check-type uri string)
  (let ((id (second (split-uri uri))))
    (unless (survey-id-p (parse-integer id))
      (error "Wrong survey id"))
    id))

(defun survey-properties (id)
  (check-type id integer)
  (first (rest (assoc id (load-response (make-surveys-db-path))))))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let ((id (survey-id (request-uri*))))
    (ml-survey/views:survey id (survey-properties (parse-integer id)))))
