;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/survey
  (:use #:cl)
  (:import-from #:hunchentoot
                #:define-easy-handler)
  (:import-from #:dev.metalisp.sbt
                #:with-page
		        #:body-header)
  (:export #:survey-id
           #:survey
           #:survey-properties-title
           #:survey-properties-description))

(in-package #:ml-survey/survey)

(defclass survey ()
  ((id :initarg :id :reader survey-id)
   (data-dir :initarg :data-dir :reader survey-data-dir)
   (properties :initarg :properties :reader survey-properties)))

(defmethod initialize-instance :after ((survey survey) &key)
  (with-slots (id data-dir properties) survey
    (setf data-dir (uiop:merge-pathnames*
                    (format nil "~a/" id)
                    (ml-survey/fileops:ensure-surveys-dir)))
    (setf properties (first (rest (assoc (parse-integer id)
                                         (ml-survey/fileops:read-from-file (ml-survey/fileops:make-surveys-db-file))))))))

(defgeneric survey-id-p (survey)
  (:documentation "Check if the survey ID is present in the surveys database."))

(defgeneric survey-data-dir-files (survey)
  (:documentation "Get the list of files in the survey's data directory."))

(defgeneric survey-data-dir-p (survey)
  (:documentation "Check if the survey's data directory exists."))

(defgeneric survey-properties-title (survey)
  (:documentation "Get title property."))

(defgeneric survey-properties-description (survey)
  (:documentation "Get description property."))

(defmethod survey-id-p ((survey survey))
  (let ((ids (mapcar #'car (read-from-file (ml-survey/fileops:make-surveys-db-file)))))
    (if (member (parse-integer (survey-id survey)) ids) t nil)))

(defmethod survey-data-dir-files ((survey survey))
  (uiop:directory-files (survey-data-dir survey)))

(defmethod survey-data-dir-p ((survey survey))
  (uiop:directory-exists-p (survey-data-dir survey)))

(defmethod survey-properties-title ((survey survey))
  (cdr (assoc "title" (survey-properties survey) :test #'string-equal)))

(defmethod survey-properties-description ((survey survey))
  (cdr (assoc "description" (survey-properties survey) :test #'string-equal)))

(defun build-questionnaire-link (survey-id resource)
  (format nil "/survey/~a~a" survey-id resource))

(defmethod survey-html ((survey survey))
  (spinneret:with-html
    (:dl (loop for property in (survey-properties survey)
               for key = (car property)
               for value = (cdr property) do
                 (:dt key)
                 (cond ((string= key "questionnaire")
                        (:dd (:a :href (build-questionnaire-link (survey-id survey) value)
				 (format nil "Open Questionnaire ~a" value))))
                       (t (:dd value)))))))

(defun results-not-null (results)
  (some (lambda (x) (and (listp x) (not (null x)))) results))

(defun group-in-chunks (lst)
  "Group LST into sublists of three elements."
  (loop for i from 0 by 3 while (< i (length lst))
        collect (subseq lst i (min (+ i 3) (length lst)))))

(defun likert-results-html (results)
  (let ((sus-average (loop for sublist in (getf results :sus)
                           sum (first (last sublist)) into total
                           count sublist into count
                           finally (return (/ total count)))))
    (spinneret:with-html
      (loop for (name data) on results by #'cddr
            do (:h3 :class "py-1" (if (eq name :sus)
                                      (format nil "~a: ~,1f" name sus-average)
                                      (format nil "~a" name)))
               (:table :class "table table-hover"
                 (:caption "Questionnaire results table")
	             (:thead
	              (:tr
	               (:th :scope "col" "Time")
	               (loop for index from 1 below (length (cdr (car data)))
                         do (:th :scope "col" (format nil "Q ~a" index)))
                   (:th :scope "col" "Score")))
	             (:tbody
	              (loop for row in data
                        do (:tr (mapcar (lambda (col) (:td col)) row)))))))))

(defun mixed-results-html (results)
  (loop for result in results
        for name = (car result)
        for data = (cdr result)
        do (spinneret:with-html (:h3 :class "py-1" (format nil "~a" name))
             (:div :class "container"
                   (loop for row in (group-in-chunks data)
                         do (:div :class "row"
                                  (loop for col in row
                                        do (:ul :class "col-4 list-group py-3"
                                                (loop for entry in col
                                                      for i from 0
                                                      do (:li :class (if (zerop i)
                                                                         "list-group-item active"
                                                                         "list-group-item")
                                                              entry))))))))))

(defun view (survey &optional results)
  "Generates the view to show the survey created."
  (check-type survey survey)
  (let ((results-not-null (results-not-null results))
        (likert-results (getf results :likert))
        (mixed-results (loop for (name data) on results by #'cddr
                             unless (eq name :likert)
                             collect (cons name data))))

    (with-page (:title "Survey Details" :add-js-urls ("/app.js"))
      (body-header "Survey Details" (ml-survey/navbar:navbar-en))
      (:main :id "main-content"
	         :class "container"
	         (:p (format nil "ID: ~a" (survey-id survey)))
	         (:h2 :class "py-3" "Properties")
	         (survey-html survey)

	         (when results-not-null
               (:h2 :class "py-3" "Questionnaire Results")

               (if likert-results
                   (likert-results-html likert-results))

               (if mixed-results
                   (mixed-results-html mixed-results)))))))

(defun extract-numbers (results)
  "Extract numbers from a questionnaire RESULTS list.
Returns a list of integers."
  (check-type results list)
  (mapcar (lambda (x)
            (parse-integer (remove-if (complement #'digit-char-p)
                                      (cdr x)))) results))

(defun sus-calc-score (results)
  (check-type results list)
  (let ((counter 0))
    (mapcar (lambda (x)
              (setq counter (1+ counter))
              (if (evenp counter)
                  (- 5 x)
                  (1- x)))
            results)))

(defun sus-calc-score-per-row (results)
  (check-type results list)
  (reverse (cons (* (apply #'+ (sus-calc-score results)) 2.5) (reverse results))))

(defun sus-calc (files)
  (check-type files list)
	(cons (car files) (sus-calc-score-per-row (extract-numbers (cdr files)))))

(defun nps-calc (scores)
  "Calculate the Net Promoter Score (NPS) from a list of SCORES."
  (check-type scores list)
  (let ((promoters 0)
        (detractors 0)
        (total-responses (length scores)))
    (dolist (score scores)
      (cond
        ((>= score 9) (incf promoters))
        ((<= score 6) (incf detractors))))
    (let ((nps (* (- (/ promoters total-responses)
                     (/ detractors total-responses))
                  100)))
      nps)))

(defstruct questionnaire-result
  type
  name
  timestamp
  post-data)

(defun questionnaire-result-list-p (list)
  "Check if all elements in LIST are of type 'questionnaire-result'."
  (if (every 'questionnaire-result-p list) t nil))

(deftype questionnaire-result-list ()
  "Define a type representing a list containing only questionnaire-result instances."
  '(and list (satisfies questionnaire-result-list-p)))

(defun questionnaire-result-from-file (filename)
  "Create a 'questionnaire-result' instance from data read from the file specified by FILENAME."
  (check-type filename (or string pathname))
  (let ((data (ml-survey/fileops:read-from-file filename)))
    (make-questionnaire-result :type (getf data :type)
                               :name (getf data :name)
                               :timestamp (getf data :timestamp)
                               :post-data (getf data :post-data))))

(defun likert-calc (name args)
  "Calculate metrics based on NAME from provided ARGS."
  (case name
    (:sus (sus-calc args))
    (:nps (nps-calc args))))

(defun string->keyword (string)
  (intern (string-upcase string) :keyword))

(defun list-of-categorized-results (result-objs)
  "Categorize and process questionnaire results listed in RESULT-OBJs.
Each result must conform to 'questionnaire-result-list' type."
  (declare (type questionnaire-result-list result-objs))
  (let ((categorized-results (list :likert nil)))
    (dolist (result result-objs categorized-results)
      (let ((name (string->keyword (questionnaire-result-name result)))
            (type (string->keyword (questionnaire-result-type result)))
            (data (questionnaire-result-post-data result))
            (timestamp (questionnaire-result-timestamp result)))
        (cond
          ((eq type :likert)
           (setf (getf (getf categorized-results :likert) name)
                 (cons (likert-calc name (cons timestamp data))
                       (getf (getf categorized-results :likert) name))))
          (t
           (setf (getf categorized-results name)
                 (cons (cons timestamp (mapcar #'cdr data))
                       (getf categorized-results name)))))))))

(defun survey-uri-p (uri)
  (let ((parts (ml-survey/app:split-uri uri)))
        (and (= (length parts) 2)
             (string= (first parts) "survey")
             (every #'digit-char-p (second parts)))))

(defun survey-uri (request)
  (survey-uri-p (hunchentoot:request-uri request)))

(define-easy-handler (survey-handler :uri #'survey-uri) ()
  (let* ((s (make-instance 'survey
                           :id (ml-survey/app:extract-from (hunchentoot:request-uri*) :survey-id)))
         (result-objs (mapcar 'questionnaire-result-from-file
                              (survey-data-dir-files s))))

    (view s (list-of-categorized-results result-objs))))
