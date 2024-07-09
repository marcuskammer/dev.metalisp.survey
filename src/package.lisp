;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey
  (:use #:cl)
  (:import-from #:hunchentoot
                #:easy-acceptor)
  (:export
   #:main
   #:*html-lang*
   #:survey
   #:survey-id
   #:survey-data-dir-p
   #:survey-data-dir-files
   #:survey-html
   #:survey-properties
   #:survey-properties-title
   #:survey-properties-description
   #:ensure-data-dir
   #:ensure-data-file-exist
   #:ensure-questionnaires-dir
   #:questionnaires-dir
   #:questionnaires-list-files
   #:extract-lang-and-filename
   #:list-questionnaires
   #:questionnaire-result
   #:questionnaire-result-from-file
   #:sus-calc
   #:write-to-file
   #:read-from-file
   #:make-surveys-db-file
   #:start
   #:*app*
   #:set-default-directory
   #:start-server
   #:stop-server
   #:restart-server))
