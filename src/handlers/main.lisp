(in-package :ml-survey/handlers)

(defun surveys-data-dir ()
  (let ((data-dir (uiop:merge-pathnames* "data/surveys/")))
    (ensure-directories-exist data-dir)
    data-dir))

(defun split-uri (uri)
  (check-type uri string)
  (remove-if #'string-empty-p
             (uiop:split-string uri :separator "/")))

(defun get-resource-id (resource request)
  (case resource
    (survey (second (split-uri request)))))

(defun get-survey-id (request)
  (get-resource-id 'survey request))

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

(defun ensure-file-exist (pathname)
  "Ensure that a file specified by PATHNAME exists, create it if it doesn't."
  (unless (uiop:file-exists-p pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (format stream "")))
  pathname)

(defun make-db-file (file)
  "Prepare and ensure a database file at FILE-STR path."
  (check-type file string)
  (let ((path (uiop:merge-pathnames* file)))
    (ensure-file-exist path)))

(defun make-surveys-db-file ()
  (make-db-file "surveys-db.lisp"))

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

(defun generate-random-id ()
  (let ((charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (length 11))
    (coerce (loop repeat length
                  collect (char charset (random (length charset))))
            'string)))

(defun string-empty-p (string) (= (length string) 0))
