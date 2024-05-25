(defpackage dev.metalisp.survey
  (:use #:cl)
  (:import-from #:hunchentoot
                #:define-easy-handler
                #:easy-acceptor
                #:post-parameters*
                #:content-type*
                #:*request*)
  (:import-from #:spinneret
                #:*html*
                #:*html-lang*)
  (:import-from #:dev.metalisp.sbt
                #:with-page)
  (:import-from #:dev.metalisp.sbt
                #:find-l10n)
  (:import-from #:dev.metalisp.sbt
                #:*l10n*)
  (:import-from #:dev.metalisp.sbt/utility
                #:spacing)
  (:import-from #:dev.metalisp.sbt/form
                #:multi-form)
  (:import-from #:dev.metalisp.sbt/btn
                #:btn-primary))