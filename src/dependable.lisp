(defpackage #:fancybuild
  (:use #:cl)
  (:export #:buildable #:dependencies #:cached-snapshot #:pretty-name #:unique-key #:snapshot #:do-build #:is-dirty))

; package in which build scripts run
(defpackage #:fancily-built
  (:use #:cl #:fancybuild))

(in-package :fancybuild)

(defclass buildable ()
  ((dependencies :initarg :dependencies :initform nil)
   (cached-snapshot :initform nil)
   (pretty-name :initarg :pretty-name :initform "")
   (unique-key :initform nil)))

(defgeneric snapshot (buildable))
(defgeneric is-dirty (buildable old))
(defgeneric do-build (buildable))

