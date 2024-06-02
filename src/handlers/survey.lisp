(in-package :ml-survey/handlers)

(defun survey-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (check-type uri string)
  (let ((parts (split-uri uri))
        (survey (make-survey uri)))
    (and (= (length parts) 2)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (funcall survey 'id-p))))

(defun survey-uri (request)
  (let ((uri (request-uri request)))
    (survey-uri-p uri)))

(defun make-survey (uri)
  (labels ((survey-fn (action)
             (case action
               (id (second (split-uri uri)))
               (id-p (let ((ids (mapcar #'car (load-response (make-surveys-db-path)))))
                       (if (member (parse-integer (survey-fn 'id)) ids) t nil)))
               (uri-p (let ((parts (split-uri uri)))
                        (and (= (length parts) 2)
                             (string= (first parts) "survey")
                             (every #'digit-char-p (second parts))
                             (survey-fn 'id-p))))
               (properties (first (rest (assoc (parse-integer (survey-fn 'id))
                                               (load-response (make-surveys-db-path)))))))))
    #'survey-fn))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let ((survey (make-survey (request-uri*))))
    (ml-survey/views:survey (funcall survey 'id) (funcall survey 'properties))))
