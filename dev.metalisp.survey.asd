(defsystem "dev.metalisp.survey"
  :description "A simple survey"
  :version "0.0.1"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev"
  :source-control "git@git.sr.ht:~marcuskammer/dev.metalisp.survey"
  :licence "MIT"
  :depends-on ("local-time" "hunchentoot" "dev.metalisp.sbt")
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "app")))
               (:module "src/views"
                :components
                ((:file "partials/_navbar")
                 (:file "forms/sus")
                 (:file "create-survey")
                 (:file "new-survey")
                 (:file "survey")
                 (:file "surveys")
                 (:file "questionnaire-submit")))
               (:module "src/handlers"
                :components
                ((:file "main")
                 (:file "create-survey")
                 (:file "new-survey")
                 (:file "survey")
                 (:file "surveys")
                 (:file "questionnaire")
                 (:file "questionnaire-submit")))))
