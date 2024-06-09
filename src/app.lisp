(in-package :ml-survey)

(defun set-default-directory (directory)
  (setf *default-pathname-defaults* (truename (merge-pathnames directory))))

(defun create-server (name port &key address document-root)
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address address
                                 :name name
                                 :document-root document-root
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
                             (merge-pathnames (uiop:getcwd) "public")))

(defun start ()
  (start-server *app*))
