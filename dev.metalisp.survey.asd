(defsystem "dev.metalisp.survey"
  :description "A simple survey"
  :version "0.0.1"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev"
  :source-control "git@git.sr.ht:~marcuskammer/dev.metalisp.survey"
  :licence "MIT"
  :depends-on ("local-time" "hunchentoot" "dev.metalisp.sbt")
  :components ((:file "package")
               (:module "src/views/partials"
                :components
                ((:file "_navbar")))
               (:module "src/views/forms"
                :components
                ((:file "sus")))
               (:module "src/views"
                :components
                ((:file "create-survey")
                 (:file "new-survey")
                 (:file "survey")
                 (:file "surveys")
                 (:file "questionnaire-submit")))
               (:module "src"
                :depends-on ("package" "src/views")
                :serial t
                :components
                ((:file "main")
                 (:file "handlers")))))
