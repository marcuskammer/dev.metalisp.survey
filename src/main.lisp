(in-package :ml-survey)

(defun create-server (name port &key address document-root)
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address address
                                 :name name
                                 :document-root document-root
                                 :port port)))
    acceptor))

(defun start-server (acceptor &key document-root)
  (if document-root
      (setf (hunchentoot:acceptor-document-root acceptor) document-root))
  (hunchentoot:start acceptor))

(defun stop-server (acceptor)
  (hunchentoot:stop acceptor))

(defun restart-server (acceptor)
  (hunchentoot:stop acceptor)
  (hunchentoot:start acceptor))

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
  (with-open-file (stream db
                          :direction :input
                          :if-does-not-exist :create)
    (if (= (file-length stream) 0)
        (list)
        (read stream))))

(defun store-response (db responses)
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

(defvar *app* (create-server 'app
                             8080
                             :document-root
                             "~/quicklisp/local-projects/dev.metalisp.survey/"))
