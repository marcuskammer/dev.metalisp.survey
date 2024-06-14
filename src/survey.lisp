(in-package :ml-survey)

(defun make-surveys-db-file ()
  (make-db-file "surveys-db.lisp"))

(defclass survey ()
  ((id :initarg :id :reader survey-id)
   (data-dir :initarg :data-dir :reader survey-data-dir)
   (properties :initarg :properties :reader survey-properties)))

(defmethod initialize-instance :after ((survey survey) &key)
  (with-slots (id data-dir properties) survey
    (setf data-dir (uiop:merge-pathnames*
                    (format nil "~a/" id)
                    (ensure-data-dir)))
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

(defun build-questionnaire-link (survey-id resource)
  (format nil "/survey/~a/questionnaire~a" survey-id resource))

(defmethod survey-html ((survey survey))
  (spinneret:with-html
    (:dl (loop for property in (survey-properties survey)
               for key = (car property)
               for value = (cdr property) do
                 (:dt key)
                 (cond ((string= key "questionnaire")
                        (:dd (:a :href (build-questionnaire-link (survey-id survey) value) value)))
                       (t (:dd value)))))))
