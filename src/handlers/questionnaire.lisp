;;;; -*- mode: common-lisp; coding: utf-8; -*-

(in-package :ml-survey/handlers)

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/lang/type'"
  (let ((parts (split-uri uri)))
    (and (= (length parts) 4)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (= 2 (length (third parts))))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (request-uri request)))

(defun process-questionnaire-get (lang questionnaire)
  (check-type lang string)
  (setf ml-survey:*html-lang* lang)
  (ml-survey/views:questionnaire questionnaire))

(defun process-questionnaire-post (request survey questionnaire)
  (let* ((post-params (post-parameters* request))
         (survey-id (ml-survey:survey-id survey))
         (questionnaire-id (generate-uuid))
         (questionnaire-data-file (ensure-data-file-exist survey-id
                                                          questionnaire-id)))

    (store-response questionnaire-data-file
                    (list :type questionnaire
                          :timestamp (today+now)
                          :post-data post-params))

    (ml-survey/views:questionnaire-submit)))

(define-easy-handler (questionnaire :uri #'questionnaire-uri) nil
  (let ((s (make-instance 'ml-survey:survey
                          :id (extract-from (request-uri*) :survey-id)))
        (language (extract-from (request-uri*) :language))
        (questionnaire (extract-from (request-uri*) :questionnaire)))
    (cond ((eq (hunchentoot:request-method*) :get)
           (process-questionnaire-get language questionnaire))
          ((eq (hunchentoot:request-method*) :post)
           (process-questionnaire-post *request* s questionnaire)))))
