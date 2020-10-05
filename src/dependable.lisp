(defpackage #:fancybuild
  (:use #:cl)
  (:export #:main))

(in-package :fancybuild)

(defclass buildable ()
  ((dependencies :initarg :dependencies :initform nil)
   (cached-snapshot :initform nil)
   (pretty-name :initarg :pretty-name :initform "")
   (unique-key :initform nil)))

(defgeneric snapshot (buildable))
(defgeneric is-dirty (buildable old))
(defgeneric do-build (buildable))

