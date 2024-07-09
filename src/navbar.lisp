;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-survey/navbar
  (:use :cl)
  (:export #:navbar-en))

(in-package :ml-survey/navbar)

(defmacro navbar-brand (src width)
  `(spinneret:with-html
     (:a :class "navbar-brand"
         :href "#"
         (:img :src ,src
               :alt "Company Logo"
               :width ,width))))

(defmacro navbar-toggle (target)
  `(spinneret:with-html
     (:button :class "navbar-toggler"
              :type "button"
              :data-bs-toggle "collapse"
              :data-bs-target (format nil "#~a" ,target)
              :aria-controls ,target
              :aria-expanded "false"
              :aria-label "Toggle Navigation"
              (:span :class "navbar-toggler-icon"))))

(defmacro navbar-nav (id &body body)
  `(spinneret:with-html
     (:div :class "collapse navbar-collapse"
           :id ,id
           (:ul :class "navbar-nav"
                ,@(loop for navitem on body by #'cddr
                        collect
                        `(:li :class "nav-item"
                              (:a :class "nav-link"
                                  :href ,(first navitem)
                                  ,(second navitem))))))))

(defmacro navbar (&body body)
  `(spinneret:with-html
     (:nav :class "navbar bg-body-tertiary navbar-expand-sm mb-5"
           :aria-label "Primary Navigation"
           (:div :class "container"
                 ,@body))))

(defmacro navbar* (id brand-src brand-width &body body)
  `(navbar (navbar-brand ,brand-src ,brand-width)
     (navbar-toggle ,id)
     (navbar-nav ,id ,@body)))

(defmacro navbar-de ()
  (let ((id "foo-bar"))
    `(navbar (navbar-brand "/company_logo.png" 30)
       (navbar-toggle ,id)
       (navbar-nav ,id "/" "Home" "/imprint" "Impressum"))))

(defmacro navbar-en ()
  (let ((id "foo-bar"))
    `(navbar (navbar-brand "/company_logo.png" 30)
       (navbar-toggle ,id)
       (navbar-nav ,id "/" "Home" "/imprint" "Imprint"))))
