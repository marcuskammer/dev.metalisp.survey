(defsystem "dev.metalisp.survey"
  :description "A simple survey"
  :version "0.0.1"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev"
  :source-control "git@git.sr.ht:~marcuskammer/dev.metalisp.survey"
  :licence "MIT"
  :depends-on ("local-time" "hunchentoot" "dev.metalisp.sbt")
  :components ((:file "package")
               (:module "src/partials"
                :components
                ((:file "_navbar")))
               (:module "src/forms"
                :components
                ((:file "sus")))
               (:module "src"
                :depends-on ("package" "src/partials" "src/forms")
                :serial t
                :components
                ((:file "pages")
                 (:file "main")))))
