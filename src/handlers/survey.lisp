(in-package :ml-survey/handlers)

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

(defun survey-uri (request)
  (let ((survey (make-survey (request-uri request))))
    (funcall survey 'uri-p)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let ((survey (make-survey (request-uri*))))
    (ml-survey/views:survey (funcall survey 'id) (funcall survey 'properties))))
