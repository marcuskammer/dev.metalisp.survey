(in-package :dev.metalisp.survey)

(defun create-server (name port &optional document-root)
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :name name
                                 :document-root document-root
                                 :port port)))
    acceptor))

(defun start-server (acceptor)
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

(defun make-db-path (&optional date-str)
  (pathname (concatenate 'string date-str "_survey-db.cl")))

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

(defun return-sus-form (lang)
  (cond ((string= lang "en")
         (sus-form-en))))

(define-easy-handler (index :uri "/") (lang)
  (setf *html-lang* lang)
  (return-sus-form lang))

(define-easy-handler (submit :uri "/submit") nil
  (setf (content-type*) "text/plain")
  (let* ((post-params (post-parameters* *request*))
         (stored-response (load-response (make-db-path (today))))
         (response (reverse (push (list (now) post-params) stored-response))))
    (store-response (make-db-path (today)) (reverse response))
    (format nil "~A" response)))
