(in-package :ml-survey/views)

(defparameter *use-cdn* nil)

(defmacro with-form ((&key
                        meta
                        (title "Web page")
                        add-css-urls
                        add-js-urls)
                     &body body)
  "This macro simplifies the process of creating an HTML web page.

META: The meta-information for the web page.

TITLE: Specifies the title of the web page. Defaults to 'Web page'.

MAIN-CON: If t add css class `container` to <main>.

ADD-CSS-URLS: An optional parameter for additional CSS file URLs.

ADD-JS-URLS: An optional parameter for additional JavaScript file URLs.

BODY: Denotes the markup for the body of the web page.

Example usage:
   (with-form (:meta (:author \"John Doe\") :title \"My Page\") \"foo\")"
  `(spinneret:with-html-string
     (:doctype)
     (:html :data-bs-theme ,dev.metalisp.sbt:*color-theme*
            (:head (:meta :charset "utf-8")
                   (:meta :name "viewport"
                          :content "width=device-width, initial-scale=1")
                   ,@(loop for (key value) on meta by #'cddr
                           collect `(:meta :name
                                           ,(string-downcase (symbol-name key))
                                           :content ,(getf meta key)))

                   (:title ,title)

                   (:link :type "text/css" :rel "stylesheet" :href ,(dev.metalisp.sbt:bs-url-css))
                   ,@(loop for url in add-css-urls
                           collect `(:link :type "text/css"
                                      :rel "stylesheet" :href ,url)))

            (:body

              (:div :class "container text-center py-3"
                    (:a :href "#main-content"
                        :class "skip-link"
                        (find-l10n "skip-link" spinneret:*html-lang* *l10n*)))

              ,@body

              (:script :src ,(dev.metalisp.sbt:bs-url-js))
              ,@(loop for url in add-js-urls
                      collect `(:script :src ,url))))))
