(defpackage ml-survey/views
  (:use #:cl)
  (:import-from #:hunchentoot
                #:request-uri*)
  (:import-from #:spinneret
                #:*html*)
  (:import-from #:dev.metalisp.sbt
                #:find-l10n
                #:*l10n*
                #:*use-cdn*
                #:with-page
		        #:body-header
		        #:body-main)
  (:import-from #:dev.metalisp.sbt/btn
                #:btn
                #:btn-primary)
  (:import-from #:dev.metalisp.sbt/form
                #:multi-form)
  (:import-from #:dev.metalisp.sbt/utility
                #:spacing)
  (:export #:index
           #:imprint
           #:new-survey
           #:surveys
           #:create-survey
           #:survey
           #:questionnaire-submit
           #:sus-form))
