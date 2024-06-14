(in-package :ml-survey)

(defun set-default-directory (directory)
  (setf *default-pathname-defaults* (truename (merge-pathnames directory))))

(defun create-server (name port &key address document-root access-log-destination)
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address address
                                 :name name
                                 :document-root document-root
                                 :access-log-destination access-log-destination
                                 :port port)))
    acceptor))

(defun start-server (acceptor &key document-root)
  (if document-root
      (setf (hunchentoot:acceptor-document-root acceptor) document-root))
  (hunchentoot:start acceptor))

(defun stop-server (acceptor)
  (hunchentoot:stop acceptor))

(defun restart-server (acceptor)
  (hunchentoot:stop acceptor)
  (hunchentoot:start acceptor))

(defvar *app* (create-server 'app
                             8080
                             :document-root
                             "public/"
                             :access-log-destination
                             (uiop:merge-pathnames* #P"access.log" (app-dir))))

(defun start ()
  (start-server *app*))
