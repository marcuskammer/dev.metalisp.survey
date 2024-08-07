;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/fileops
  (:use #:cl)
  (:export #:public-dir
           #:access-log-file
           #:read-from-file
           #:write-to-file
           #:ensure-questionnaires-dir
           #:ensure-data-file-exist
           #:ensure-surveys-dir
           #:make-db-file
           #:extract-lang-and-filename
           #:questionnaires-list-files
           #:questionnaires-dir
           #:make-surveys-db-file))

(in-package #:ml-survey/fileops)

(defun data-dir ()
  "Construct and return the directory path for storing data within the
application."
  (uiop:merge-pathnames* #P"ml-survey/" (uiop:xdg-data-home)))

(defun surveys-dir ()
  "Construct and return the directory path for storing surveys within the
application."
  (uiop:merge-pathnames* #P"surveys/" (data-dir)))

(defun questionnaires-dir ()
  "Construct and return the directory path for storing forms within the
application."
  (uiop:merge-pathnames* #P"questionnaires/" (data-dir)))

(defun ensure-data-dir ()
  "Ensure the data directory exists, create it if necessary, and return its
path."
  (ensure-directories-exist (data-dir)))

(defun ensure-surveys-dir ()
  "Ensure the data directory exists, create it if necessary, and return its
path."
  (ensure-directories-exist (surveys-dir)))

(defun ensure-questionnaires-dir ()
  "Ensure the data directory exists, create it if necessary, and return its
path."
  (ensure-directories-exist (questionnaires-dir)))

(defun questionnaires-list-files ()
  (uiop:directory* (format nil "~a*/*.lisp" (questionnaires-dir))))

(defun find-next-directory (dir-list target)
  (let ((index (position target dir-list :test #'string=)))
    (when index
      (nth (1+ index) dir-list))))

(defun extract-lang-and-filename (path &optional (target-dir "questionnaires"))
  (let* ((directory (pathname-directory path))
         (name (pathname-name path))
         (lang (find-next-directory directory target-dir)))
    (if lang
        (format nil "/~A/~A" lang name)
        (format nil "/~A" name))))

(defun ensure-file-exist (pathname)
  "Ensure that a file specified by PATHNAME exists, create it if it doesn't."
  (unless (uiop:file-exists-p pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (format stream "")))
  (truename pathname))

(defun ensure-data-file-exist (survey-id questionnaire-id)
  "Ensure that a specific file for given survey and questionnaire IDs exists
within the data directory."
  (let ((path (format nil "~a~a/~a.lisp"
                      (ensure-surveys-dir)
                      survey-id
                      questionnaire-id)))
    (ensure-file-exist (ensure-directories-exist path))))

(defun make-db-file (file)
  "Prepare and ensure a database file at FILE-STR path."
  (check-type file string)
  (let ((path (uiop:merge-pathnames* file (ensure-surveys-dir))))
    (ensure-file-exist path)))

(defun read-from-file (db)
  (check-type db (or string pathname))
  (with-open-file (stream db
                          :direction :input
                          :if-does-not-exist :create)
    (if (= (file-length stream) 0)
        (list)
        (read stream))))

(defun write-to-file (db responses)
  (check-type db (or string pathname))
  (check-type responses list)
  (with-open-file (stream db
                          :direction :output
                          :if-exists :supersede)
    (prin1 responses stream)))

(defun access-log-file ()
  (uiop:merge-pathnames* #P"access.log" (data-dir)))

(defun public-dir ()
  (uiop:merge-pathnames* #P"public/" (data-dir)))

(ensure-directories-exist (public-dir))

(format t "~%App Data Directory: ~a~%" (data-dir))

(defun make-surveys-db-file ()
  (make-db-file "surveys-db.lisp"))
