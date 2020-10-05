(defpackage #:fancybuild
  (:use #:cl)
  (:export #:buildable #:dependencies #:cached-snapshot #:pretty-name #:unique-key #:snapshot #:do-build #:is-dirty))

; package in which build scripts run
(defpackage #:fancily-built
  (:use #:cl #:fancybuild))

(in-package :fancybuild)

(defclass buildable ()
  ((dependencies :initarg :dependencies :initform nil :accessor dependencies)
   (cached-snapshot :initform nil)
   (pretty-name :initarg :pretty-name :initform "" :accessor pretty-name)
   (unique-key :initform nil :accessor unique-key)))

(defgeneric snapshot (buildable))
(defgeneric is-dirty (buildable old))
(defgeneric do-build (buildable))

(defmethod snapshot ((b buildable)))
(defmethod is-dirty ((b buildable) o) t)
(defmethod do-build ((b buildable)))

(defun check-dirty (b)
  (or (null (slot-value b 'cached-snapshot))
      (is-dirty b (slot-value b 'cached-snapshot))))
