;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defsystem "dev.metalisp.survey"
  :description "A simple survey"
  :version "0.3.0"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev>"
  :source-control "git@git.sr.ht:~marcuskammer/dev.metalisp.survey"
  :licence "MIT"
  :depends-on
  ("local-time" "hunchentoot" "dev.metalisp.sbt")
  :components
  ((:module "src"
    :components
    ((:file "fileops")
     (:file "navbar")
     (:file "app")
     (:file "survey")
     (:file "questionnaire")
     (:file "surveys")
     (:file "new-survey")))))
