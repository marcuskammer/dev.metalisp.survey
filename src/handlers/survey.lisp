(in-package :ml-survey/handlers)

(defun process-and-categorize-results (result-objs)
  "Categorize results into different lists based on their type.
  Apply special calculation for results of type 'sus'."
  (let ((categorized-results (list :sus nil :other nil)))
    (dolist (result result-objs categorized-results)
      (let ((type (ml-survey::questionnaire-result-type result))
            (data (ml-survey::questionnaire-result-post-data result))
            (timestamp (ml-survey::questionnaire-result-timestamp result)))
        (cond
          ((string= type "sus")
           (setf (getf categorized-results :sus)
                 (cons (ml-survey:sus-calc (cons timestamp data))
                       (getf categorized-results :sus))))
          (t
           (setf (getf categorized-results :other)
                 (cons (cons timestamp (mapcar #'cdr data))
                       (getf categorized-results :other)))))))))

(defun survey-uri-p (uri)
  (let ((parts (split-uri uri)))
        (and (= (length parts) 2)
             (string= (first parts) "survey")
             (every #'digit-char-p (second parts)))))

(defun survey-uri (request)
  (survey-uri-p (request-uri request)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let* ((s (make-instance 'ml-survey:survey
                           :id (extract-from (request-uri*) :survey-id)))
         (result-objs (mapcar #'ml-survey:build-questionnaire-result
                              (ml-survey:survey-data-dir-files s))))
    (ml-survey/views:survey s (process-and-categorize-results result-objs))))
