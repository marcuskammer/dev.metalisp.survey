;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/handlers
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*)
  (:import-from #:hunchentoot
                #:define-easy-handler
                #:post-parameters*
                #:content-type*
                #:request-uri
                #:request-uri*
                #:*request*)
  (:import-from #:ml-survey
                #:ensure-data-dir
                #:ensure-data-file-exist
                #:write-to-file
                #:read-from-file
                #:make-surveys-db-file))
