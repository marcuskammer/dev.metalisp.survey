(in-package :ml-survey)

(defun base-dir ()
  "Determine and return the base directory for application data based on the
operating system."
  (let ((os (uiop:detect-os)))
    (cond ((eq os :os-windows) (truename "~/AppData/Local/"))
          ((eq os :os-unix) (truename "~/.local/share/"))
          ((eq os :os-macos) (truename "~/Library/Application Support/"))
          (t (error "Unsupported OS")))))

(defun app-dir ()
  "Construct and return the application-specific directory path."
  (uiop:merge-pathnames* #P"ml-survey/" (base-dir)))

(defun data-dir ()
  "Construct and return the directory path for storing data within the
application."
  (uiop:merge-pathnames* #P"data/surveys/" (app-dir)))

(defun ensure-data-dir ()
  "Ensure the data directory exists, create it if necessary, and return its
path."
  (let ((data-dir (data-dir)))
    (ensure-directories-exist (data-dir))
  data-dir))

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
                      (ensure-data-dir)
                      survey-id
                      questionnaire-id)))
    (ensure-file-exist (ensure-directories-exist path))))

(defun make-db-file (file)
  "Prepare and ensure a database file at FILE-STR path."
  (check-type file string)
  (let ((path (uiop:merge-pathnames* file (ensure-data-dir))))
    (ensure-file-exist path)))

(defun load-response (db)
  (check-type db (or string pathname))
  (with-open-file (stream db
                          :direction :input
                          :if-does-not-exist :create)
    (if (= (file-length stream) 0)
        (list)
        (read stream))))

(defun store-response (db responses)
  (check-type db (or string pathname))
  (check-type responses list)
  (with-open-file (stream db
                          :direction :output
                          :if-exists :supersede)
    (prin1 responses stream)))

(defun access-log-file ()
  (uiop:merge-pathnames* #P"access.log" (app-dir)))

(defun public-dir ()
  (uiop:merge-pathnames* #P"public/" (app-dir)))
