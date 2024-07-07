;;;; -*- mode: common-lisp; coding: utf-8; -*-

(in-package :ml-survey/handlers)

(defun list-of-categorized-results (result-objs)
  "Categorize results into different lists based on their type.
  Apply special calculation for results of type 'sus'."
  (let ((categorized-results (list :sus nil)))
    (dolist (result result-objs categorized-results)
      (let ((type (intern (string-upcase (ml-survey::questionnaire-result-type result)) :keyword))
            (data (ml-survey::questionnaire-result-post-data result))
            (timestamp (ml-survey::questionnaire-result-timestamp result)))
        (cond
          ((eq type :sus)
           (setf (getf categorized-results :sus)
                 (cons (ml-survey:sus-calc (cons timestamp data))
                       (getf categorized-results :sus))))
          (t
           (setf (getf categorized-results type)
                 (cons (cons timestamp (mapcar #'cdr data))
                       (getf categorized-results type)))))))))

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
         (result-objs (mapcar #'ml-survey:questionnaire-result-from-file
                              (ml-survey:survey-data-dir-files s))))
    (ml-survey/views:survey s (list-of-categorized-results result-objs))))
