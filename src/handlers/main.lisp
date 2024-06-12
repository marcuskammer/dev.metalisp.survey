(in-package :ml-survey/handlers)

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
