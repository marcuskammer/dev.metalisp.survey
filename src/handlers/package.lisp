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
