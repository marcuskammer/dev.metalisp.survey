(defpackage ml-survey
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
  (:import-from #:dev.metalisp.sbt/utility
                #:spacing))

(defpackage ml-survey/forms
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
  (:export #:sus-form-en #:sus-form-de))

(defpackage ml-survey/partials
  (:use #:cl)
  (:export #:navbar-en #:navbar-de))

(defpackage ml-survey/views
  (:use #:cl)
  (:import-from #:dev.metalisp.sbt
                #:with-page)
  (:export #:index
           #:imprint
           #:new-survey
           #:surveys
           #:create-survey
           #:survey))
