(in-package :ml-survey)

(defun base-dir ()
  (let ((os (uiop:detect-os)))
    (uiop:parse-native-namestring
     (cond ((eq os :os-windows)
            (format nil "~a/" (uiop:getenv "LOCALAPPDATA")))

           ((eq os :os-unix)
            (format nil "~a/.local/share/" (user-homedir-pathname)))

           ((eq os :os-macos)
            (format nil "~a/Library/Application Support/" (user-homedir-pathname)))

           (t (error "Unsupported OS"))))))

(defun app-dir ()
  (uiop:merge-pathnames* #P"ml-survey/" (base-dir)))

(defun data-dir ()
  (uiop:merge-pathnames* #P"data/surveys/" (app-dir)))

(defun ensure-data-dir ()
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
  pathname)

(defun ensure-data-file-exist (survey-id questionnaire-id)
  (let ((path (format nil "~a~a/~a.lisp"
                      (ensure-data-dir)
                      survey-id
                      questionnaire-id)))
    (ensure-directories-exist path)
    (ensure-file-exist path)))

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
