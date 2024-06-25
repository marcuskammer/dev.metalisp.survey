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
