(in-package :ml-survey)

(defun split-uri (uri)
  (remove-if #'string-empty-p (uiop:split-string uri :separator "/")))

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts)))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (hunchentoot:request-uri request)))

(defun return-sus-form (lang)
  "Based on LANG decide which sus form to show."
  (check-type lang string)
  (cond ((string= lang "en") #'ml-survey/forms:sus-form-en)
        ((string= lang "de") #'ml-survey/forms:sus-form-de)
        (t (error "Unsupported language: ~A" lang))))

(define-easy-handler (questionnaire :uri #'questionnaire-uri) (lang)
  (let ((survey-id (second (split-uri (hunchentoot:request-uri*)))))
    (setf *html-lang* lang)
    (funcall (return-sus-form lang) survey-id)))

(defun questionnaire-submit-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 3)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (search "submit" (third parts)))))

(defun questionnaire-submit-uri (request)
  (questionnaire-submit-uri-p (hunchentoot:request-uri request)))

(defun ensure-data-file-exist (id &optional lang)
  (ensure-directories-exist (format nil "~a~a/~a-~a.lisp"
                                    *survey-data-dir*
                                    id
                                    (generate-uuid)
                                    lang)))

(define-easy-handler (questionnaire-submit :uri #'questionnaire-submit-uri) (lang)
  (let ((post-params (post-parameters* *request*))
        (id (second (split-uri (hunchentoot:request-uri*)))))
    (store-response (ensure-data-file-exist id) post-params)
    (ml-survey/views:questionnaire-submit)))

(defun survey-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 2)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts)))))

(defun survey-uri (request)
  (let ((uri (hunchentoot:request-uri request)))
    (survey-uri-p uri)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let* ((id (subseq (hunchentoot:request-uri*) (length "/survey/")))
         (survey (assoc (parse-integer id)
                        (load-response (make-surveys-db-path)))))
    (ml-survey/views:survey survey)))

(define-easy-handler (new-survey :uri "/new-survey") nil
  (ml-survey/views:new-survey))

(define-easy-handler (create-survey :uri "/create-survey") nil
  (let ((post-params (post-parameters* *request*))
        (uid (ml-survey:generate-uuid))
        (stored-surveys (load-response (make-surveys-db-path))))
    (store-response (make-surveys-db-path) (push (list uid post-params) stored-surveys))
    (ml-survey/views:create-survey uid)))

(define-easy-handler (surveys :uri "/") nil
  (let ((stored-surveys (load-response (make-surveys-db-path))))
    (hunchentoot:start-session)
    (ml-survey/views:surveys stored-surveys)))
