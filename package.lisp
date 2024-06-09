(defpackage ml-survey
  (:use #:cl)
  (:import-from #:hunchentoot
                #:easy-acceptor)
  (:export
   #:start
   #:*app*
   #:set-default-directory
   #:start-server
   #:stop-server
   #:restart-server))

(defpackage ml-survey/views
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*
                #:*html-lang*)
  (:import-from #:dev.metalisp.sbt
                #:find-l10n)
  (:import-from #:dev.metalisp.sbt
                #:*l10n*)
  (:import-from #:dev.metalisp.sbt/btn
                #:btn
                #:btn-primary)
  (:import-from #:dev.metalisp.sbt
                #:with-page)
  (:import-from #:dev.metalisp.sbt/form
                #:multi-form)
  (:export #:index
           #:imprint
           #:new-survey
           #:surveys
           #:create-survey
           #:survey
           #:questionnaire-submit
           #:sus-form-de
           #:sus-form-en))

(defpackage ml-survey/handlers
  (:use #:cl)
  (:import-from #:spinneret
                #:*html*
                #:*html-lang*)
  (:import-from #:hunchentoot
                #:define-easy-handler
                #:post-parameters*
                #:content-type*
                #:request-uri
                #:request-uri*
                #:*request*))
