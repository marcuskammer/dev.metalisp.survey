(ql:quickload '(:local-time :hunchentoot :dev.metalisp.sbt))

(defpackage dev.metalisp.survey
  (:use #:common-lisp)
  (:import-from hunchentoot #:define-easy-handler #:easy-acceptor #:post-parameters* #:content-type* #:*request*)
  (:import-from spinneret #:*html* #:*html-lang*)
  (:import-from dev.metalisp.sbt #:with-page)
  (:import-from dev.metalisp.sbt #:find-l10n)
  (:import-from dev.metalisp.sbt #:*l10n*)
  (:import-from dev.metalisp.sbt/utility #:spacing)
  (:import-from dev.metalisp.sbt/form #:multi-form)
  (:import-from dev.metalisp.sbt/btn #:btn-primary))

(in-package #:dev.metalisp.survey)

(defvar *company-logo* "company_logo.png")

(defun sus-form-en ()
  (with-page (:title "SUS Form" :main-con t)
    (:nav :class "navbar bg-body-tertiary"
          (:div :class "container"
                (:a :class "navbar-brand" :href "#"
                    (:img :src *company-logo* :alt "Company Logo" :width 100))))
    (:h2 "Usability Feedback Form")
    (:p "Please fill out the following forms and press the submit button.")
    (:form :action "/submit"
           :method "post"
           :class (spacing :property "m" :side "y" :size 5)
           (multi-form
             (:ask "I’d like to use this system frequently."
              :group "sus-1"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The system is unnecessarily complex."
              :group "sus-2"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The system is easy to use."
              :group "sus-3"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "I need the support of a technical person to use this system."
              :group "sus-4"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The functions in this system are well integrated."
              :group "sus-5"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "There is too much inconsistency in this system."
              :group "sus-6"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "Most people would learn to use this system very quickly."
              :group "sus-7"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "The system is very awkward to use."
              :group "sus-8"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "I feel very confident using this system."
              :group "sus-9"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree"))

             (:ask "I needed to learn a lot of things to get started with this system."
              :group "sus-10"
              :style "list-style:none;"
              :choices (:single "1 Strongly Disagree"
                                "2 Disagree"
                                "3 Neither Agree nor Disagree"
                                "4 Agree"
                                "5 Strongly Agree")))

           (btn-primary (:type "submit")
             (find-l10n "submit" *html-lang* *l10n*)))))

(defun handle-acceptor (acceptor)
  (lambda (action)
    (case action
      (start (hunchentoot:start acceptor))
      (stop (hunchentoot:stop acceptor))
      (restart (progn (hunchentoot:stop acceptor)
                      (hunchentoot:start acceptor))))))

(defun today ()
  "Return today's date formatted as ISO-8601."
  (local-time:format-timestring nil (local-time:now) :format '(:year "-" (:month 2) "-" (:day 2))))

(defun now ()
  "Return current time formatted as ISO-8601."
  (local-time:format-timestring nil (local-time:now) :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))

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

(define-easy-handler (sus :uri "/") ((lang) (name))
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
