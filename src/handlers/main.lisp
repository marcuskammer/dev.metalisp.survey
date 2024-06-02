(in-package :ml-survey/handlers)

(defvar *survey-data-dir*
  (ensure-directories-exist (format nil
                                    "~adata/survey/"
                                    (uiop:getcwd))))

(defun split-uri (uri)
  (check-type uri string)
  (remove-if #'string-empty-p
             (uiop:split-string uri :separator "/")))

(defun valid-survey-id-p (id)
  (check-type id integer)
  (let ((ids (mapcar #'car (load-response (make-surveys-db-path)))))
    (if (member id ids) t nil)))

(defun today ()
  "Return today's date formatted as ISO-8601."
  (local-time:format-timestring nil
                                (local-time:now)
                                :format '(:year "-" (:month 2) "-" (:day 2))))

(defun now ()
  "Return current time formatted as ISO-8601."
  (local-time:format-timestring nil
                                (local-time:now)
                                :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun make-db-path (date-str file-str)
  (pathname (concatenate 'string date-str file-str)))

(defun make-surveys-db-path ()
  (make-db-path (today) "-surveys-db.lisp"))

(defun load-response (db)
  (check-type db (or string pathname))
  (with-open-file (stream db
                          :direction :input
                          :if-does-not-exist :create)
    (if (= (file-length stream) 0)
        (list)
        (read stream))))

(defun store-response (db responses)
  (check-type db (or string pathname))
  (check-type responses list)
  (with-open-file (stream db
                          :direction :output
                          :if-exists :supersede)
    (prin1 responses stream)))

(defun generate-uuid ()
  (parse-integer (format nil "~A~A~A"
                         (sb-posix:getpid)
                         (get-universal-time)
                         (random 1000000))))

(defun string-empty-p (string) (= (length string) 0))
