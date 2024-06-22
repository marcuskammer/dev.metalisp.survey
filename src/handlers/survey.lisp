(in-package :ml-survey/handlers)

(defun extract-numbers (results)
  "Extract numbers from a questionnaire RESULTS list.
Returns a list of integers."
  (check-type results list)
  (mapcar (lambda (x)
            (parse-integer (remove-if (complement #'digit-char-p)
                                      (cdr x)))) results))

(defun sus-calc-score (results)
  (check-type results list)
  (let ((counter 0))
    (mapcar (lambda (x)
              (setq counter (1+ counter))
              (if (evenp counter)
                  (- 5 x)
                  (1- x)))
            results)))

(defun sus-calc-score-per-row (results)
  (check-type results list)
  (reverse (cons (* (apply #'+ (sus-calc-score results)) 2.5) (reverse results))))

(defun sus-calc (files)
  (check-type files list)
  (loop for f in files
        for resp = (load-response f)
        collect
	(sus-calc-score-per-row (extract-numbers resp))))

(defun survey-uri-p (uri)
  (let ((parts (split-uri uri)))
        (and (= (length parts) 2)
             (string= (first parts) "survey")
             (every #'digit-char-p (second parts)))))

(defun survey-uri (request)
  (survey-uri-p (request-uri request)))

(define-easy-handler (survey :uri #'survey-uri) ()
  (let ((s (make-instance 'ml-survey:survey :id (get-survey-id (request-uri*)))))
    (ml-survey/views:survey s
                            (when (ml-survey:survey-data-dir-p s)
                              (sus-calc (ml-survey:survey-data-dir-files s))))))
