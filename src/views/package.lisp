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
           #:sus-form))
