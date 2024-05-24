(in-package :dev.metalisp.survey)

(defvar *company-logo* "company_logo.png")

(defun handle-acceptor (acceptor)
  (lambda (action)
    (case action
      (start (hunchentoot:start acceptor))
      (stop (hunchentoot:stop acceptor))
      (restart (progn (hunchentoot:stop acceptor)
                      (hunchentoot:start acceptor))))))

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

(defvar *db* (pathname (concatenate 'string (today) "_survey-db.cl")))

(defvar *app* (handle-acceptor (make-instance 'easy-acceptor
                                              :document-root (uiop:getcwd)
                                              :port 8080)))

(defun load-response (db)
  (with-open-file (stream db
                          :direction :input
                          :if-does-not-exist :create)
    (if (= (file-length stream) 0)
        '()
        (read stream))))

(defun store-response (db responses)
  (with-open-file (stream db
                          :direction :output
                          :if-exists :supersede)
    (prin1 responses stream)))

(define-easy-handler (sus :uri "/") (lang)
  (setf *html-lang* lang)
  (cond ((string= lang "en")
         (sus-form-en))))

(define-easy-handler (submit :uri "/submit") nil
  (setf (content-type*) "text/plain")

  (let ((post-params (post-parameters* *request*))
        (stored-response (load-response *db*)))

    (let ((response stored-response))
      (push (list (now) post-params) response)
      (store-response *db* (reverse response))
      (format nil "~A" response))))
