(defsystem "dev.metalisp.survey"
  :description "A simple survey"
  :version "0.0.1"
  :author "Marcus Kammer <marcus.kammer@metalisp.dev"
  :source-control "git@git.sr.ht:~marcuskammer/dev.metalisp.survey"
  :licence "MIT"
  :depends-on ("local-time" "hunchentoot" "dev.metalisp.sbt")
  :components ((:file "main")))
