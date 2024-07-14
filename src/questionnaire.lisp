;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/questionnaire
  (:use :cl)
  (:import-from #:hunchentoot
                #:define-easy-handler)
  (:import-from #:dev.metalisp.sbt
                #:find-l10n
                #:*l10n*
                #:*use-cdn*
                #:with-page
		        #:body-header
		        #:body-main)
  (:import-from #:dev.metalisp.sbt/btn
                #:btn
                #:btn-primary)
  (:import-from #:dev.metalisp.sbt/form
                #:multi-form)
  (:import-from #:dev.metalisp.sbt/utility
                #:spacing))

(in-package :ml-survey/questionnaire)

(setf *use-cdn* nil)

(defstruct questionnaire
  (lang "" :type string)
  (name "" :type string))

(defun load-form (q)
  (declare (type questionnaire q))
  "Load a Lisp file containing form definitions."
  (let* ((form-path (uiop:merge-pathnames* (format nil "~a/~a.lisp"
                                                   (questionnaire-lang q)
                                                   (questionnaire-name q))
                                           (ml-survey/fileops:ensure-questionnaires-dir))))
    (unless (probe-file form-path)
      (error "Form file ~A does not exist." form-path))
    (load form-path))
  nil)

(defmacro with-form (&body body)
  "Create a standardized HTML form wrapped in a <main> tag with a pre-defined
class and structure, using the Spinneret library. The form is designed to be
used within a web application served by Hunchentoot, utilizing common layout
and localization practices. The macro automatically sets the form’s action to
the current request URI and expects certain functions and variables to be
available in its environment for full functionality."
  `(spinneret:with-html
    (:main :id "main-content"
           :class "container my-5"
           (:p "Please fill out the following forms and press the submit button.")
           ;; action is defined as hunchentoot:request-uri* function
           (:form :action (hunchentoot:request-uri*)
                  :method "post"
                  :class (spacing :property "m" :side "y" :size 5)
                  ,@body
                  (btn-primary (:type "submit")
                    (find-l10n "submit" ml-survey/app:*html-lang* *l10n*))))))

(defun view (q)
  (declare (type questionnaire q))
  (with-page (:title "SUS Form")
    (body-header "System Usability Form")
    (with-form (load-form q))))

(defun view-submit ()
  (with-page (:title "Confirmation")
    (body-header "Confirmation")
    (:main :id "main-content"
           :class "container"
           (:div :class "alert alert-info"
                 :role "alert"
                  "Thank you for filling out the questionnaire."))))

(defun questionnaire-uri-p (uri)
  "Check if the request URI matches the pattern '/survey/<numeric>/lang/type'"
  (let ((parts (ml-survey/app:split-uri uri)))
    (and (= (length parts) 4)
         (string= (first parts) "survey")
         (every #'digit-char-p (second parts))
         (= 2 (length (third parts))))))

(defun questionnaire-uri (request)
  (questionnaire-uri-p (hunchentoot:request-uri request)))

(defparameter *likert-scale*
  '(:sus :nps :ueq :mecue :seq :umux :pwu :smeq :intui))

(defun likert-p (q)
  (let ((q-keyword (if (stringp q) (intern (string-upcase q) :keyword) q)))
    (if (member q-keyword *likert-scale*)
        t
        nil)))

(defun process-questionnaire-get (q)
  (declare (type questionnaire q))
  (setf ml-survey/app:*html-lang* (questionnaire-lang q))
  (view q))

(defun process-questionnaire-post (request survey q)
  (declare (type questionnaire q))
  (let* ((post-params (hunchentoot:post-parameters* request))
         (survey-id (ml-survey/survey:survey-id survey))
         (questionnaire-id (ml-survey/app:generate-uuid))
         (questionnaire-data-file (ml-survey/fileops:ensure-data-file-exist survey-id
                                                                            questionnaire-id)))

    (ml-survey/fileops:write-to-file questionnaire-data-file
                                     (list :type (if (likert-p (questionnaire-name q))
                                                     "likert"
                                                     "mixed")
                                           :name (questionnaire-name q)
                                           :timestamp (ml-survey/app:today+now)
                                           :post-data post-params))

    (view-submit)))

(define-easy-handler (questionnaire-handler :uri #'questionnaire-uri) nil
  (let ((s (make-instance 'ml-survey/survey:survey
                          :id (ml-survey/app:extract-from (hunchentoot:request-uri*) :survey-id)))

        (questionnaire (make-questionnaire :lang
                                           (ml-survey/app:extract-from (hunchentoot:request-uri*)
                                                                       :lang)
                                           :name
                                           (ml-survey/app:extract-from (hunchentoot:request-uri*)
                                                                       :questionnaire))))

    (cond ((eq (hunchentoot:request-method*) :get)
           (process-questionnaire-get questionnaire))
          ((eq (hunchentoot:request-method*) :post)
           (process-questionnaire-post hunchentoot:*request* s questionnaire)))))
