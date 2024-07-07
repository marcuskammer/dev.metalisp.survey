;;;; -*- mode: common-lisp; coding: utf-8; -*-

(in-package :ml-survey)

(defun find-next-directory (dir-list target)
  (let ((index (position target dir-list :test #'string=)))
    (when index
      (nth (1+ index) dir-list))))

(defun extract-lang-and-filename (path &optional (target-dir "forms"))
  (let* ((directory (pathname-directory path))
         (name (pathname-name path))
         (lang (find-next-directory directory target-dir)))
    (if lang
        (format nil "/~A/~A" lang name)
        (format nil "/~A" name))))

(defun list-questionnaires ()
  (mapcar #'extract-lang-and-filename (questionnaires-list-files)))

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
	(cons (car files) (sus-calc-score-per-row (extract-numbers (cdr files)))))

(defstruct questionnaire-result
  type
  timestamp
  post-data)

(defun questionnaire-result-from-file (filename)
  (check-type filename (or string pathname))
  (let ((data (load-response filename)))
    (make-questionnaire-result :type (getf data :type)
                               :timestamp (getf data :timestamp)
                               :post-data (getf data :post-data))))
