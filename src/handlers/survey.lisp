(in-package :ml-survey/handlers)

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
  (* (apply #'+ (sus-calc-score results)) 2.5))

(defun sus-calc (files)
  (check-type files list)
  (loop for f in files
        for resp = (load-response f)
        collect
        (sus-calc-score-per-row (extract-numbers resp))))

(defclass survey ()
  ((id :initarg :id :reader survey-id)
   (data-dir :initarg :data-dir :reader survey-data-dir)
   (properties :initarg :properties :reader survey-properties)))

(defmethod initialize-instance :after ((survey survey) &key)
  (with-slots (id data-dir properties) survey
    (setf data-dir (uiop:merge-pathnames*
                    (format nil "~a/" id)
                    (surveys-data-dir)))
    (setf properties (first (rest (assoc (parse-integer id)
                                         (load-response (make-surveys-db-file))))))))

(defgeneric survey-id-p (survey)
  (:documentation "Check if the survey ID is present in the surveys database."))

(defgeneric survey-data-dir-files (survey)
  (:documentation "Get the list of files in the survey's data directory."))

(defgeneric survey-data-dir-p (survey)
  (:documentation "Check if the survey's data directory exists."))

(defmethod survey-id-p ((survey survey))
  (let ((ids (mapcar #'car (load-response (make-surveys-db-file)))))
    (if (member (parse-integer (survey-id survey)) ids) t nil)))

(defmethod survey-data-dir-files ((survey survey))
  (uiop:directory-files (survey-data-dir survey)))

(defmethod survey-data-dir-p ((survey survey))
  (uiop:directory-exists-p (survey-data-dir survey)))

(defmethod survey-html ((survey survey))
  (spinneret:with-html
    (:table :class "table"
      (:thead :class "thead-dark"
              (:tr (:th :scope "col"
                        "Key")
                   (:th :scope "col"
                        "Value")))
      (:tbody (loop for property in (survey-properties survey)
                    for key = (car property)
                    for value = (cdr property) do
                      (:tr (:td key)
                           (:td (if (string= key "questionnaire")
                                    (:a :href (concatenate 'string "/survey/" (survey-id survey) value)
                                        value)
                                    value))))))))

(defun survey-uri-p (uri)
  (let* ((parts (split-uri uri))
         (s (make-instance 'survey :id (second parts))))
    (and (= (length parts) 2)
         (string= (first parts) "survey")
         (every #'digit-char-p (survey-id s))
         (survey-id-p s))))

(defun survey-uri (request)
  (survey-uri-p (request-uri request)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let ((s (make-instance 'survey :id (second (split-uri (request-uri*))))))
    (ml-survey/views:survey s
                            (when (survey-data-dir-p s)
                              (sus-calc (survey-data-dir-files s))))))
