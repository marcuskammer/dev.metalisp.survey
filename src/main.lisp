(in-package :dev.metalisp.survey)

(defun create-server (name port &key address document-root)
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address address
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

(defun return-sus-form (lang)
  "Based on LANG decide which sus form to show."
  (check-type lang string)
  (cond ((string= lang "en") (dev.metalisp.survey/forms:sus-form-en))
        ((string= lang "de") (dev.metalisp.survey/forms:sus-form-de))
        (t (error "Unsupported language: ~A" lang))))

(define-easy-handler (index :uri "/") ()
  (dev.metalisp.survey/pages:index))

(define-easy-handler (imprint :uri "/imprint") ()
  (dev.metalisp.survey/pages:imprint))

(define-easy-handler (sus :uri "/sus") (lang)
  (setf *html-lang* lang)
  (return-sus-form lang))

(define-easy-handler (submit :uri "/submit") nil
  (setf (content-type*) "text/plain")
  (let* ((post-params (post-parameters* *request*))
         (stored-response (load-response (make-db-path (today) "_submit-db.lisp")))
         (response (reverse (push (list (now) post-params) stored-response))))
    (store-response (make-db-path (today) "_submit-db.lisp") response)
    (format nil "~A" response)))

(defun starts-with-subseq (subseq seq)
  "Check if the sequence SEQ starts with the subsequence SUBSEQ."
  (let ((subseq-length (length subseq)))
    (and (<= subseq-length (length seq))
         (string= subseq (subseq seq 0 subseq-length)))))

(defun survey-uri-p (request)
  "Predicate function to check if the request URI matches the survey pattern.
The URI should start with \"/survey/\" followed by a numeric ID."
  (let ((uri (hunchentoot:request-uri request)))
    (and (starts-with-subseq "/survey/" uri)
         (let ((id (subseq uri (length "/survey/"))))
           (every #'digit-char-p id)))))

(define-easy-handler (survey-handler :uri #'survey-uri-p) ()
  (let ((id (subseq (hunchentoot:request-uri*) (length "/survey/"))))
    (setf (content-type*) "text/plain")
    (format nil "Survey ID: ~a" id)))

(define-easy-handler (new-survey :uri "/new-survey") nil
  (dev.metalisp.survey/pages:new-survey))

(define-easy-handler (create-survey :uri "/create-survey"
                                   :default-request-type :post) nil
  (let ((post-params (post-parameters* *request*))
        (uid (* (get-universal-time) (random 999)))
        (stored-surveys (load-response (make-surveys-db-path))))
    (store-response (make-surveys-db-path) (push (list uid post-params) stored-surveys))
    (setf (content-type*) "text/plain")
    (format nil "~A" post-params)))
