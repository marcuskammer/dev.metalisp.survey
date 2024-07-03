(defsystem "dev.metalisp.survey"
  :description "A simple survey"
  :version "0.1.2"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev"
  :source-control "git@git.sr.ht:~marcuskammer/dev.metalisp.survey"
  :licence "MIT"
  :depends-on
  ("local-time" "hunchentoot" "dev.metalisp.sbt")
  :components

  ((:module "src"
    :components
    ((:file "package")
     (:file "fileops")
     (:file "app")
     (:file "survey")
     (:file "questionnaire")))

   (:module "src/views"
    :components
    ((:file "package")
     (:file "_navbar")
     (:file "main")
     (:file "questionnaire")
     (:file "new-survey")
     (:file "survey")
     (:file "surveys")
     (:file "questionnaire-submit")))

   (:module "src/handlers"
    :components
    ((:file "package")
     (:file "main")
     (:file "new-survey")
     (:file "survey")
     (:file "surveys")
     (:file "questionnaire")))))
