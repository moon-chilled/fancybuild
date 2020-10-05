(defpackage #:fb
  (:use :cl :asdf))

(defsystem "fb"
    :name "Fancy Build"
    ;:serial nil
    :pathname "src/"

    :depends-on (#:osicat #:ironclad #:cl-conspack)

    :components ((:file "dependable")
		 (:file "fb-entry" :depends-on ("dependable")))
		 ;(:file "x" :depends-on ("y" "z"))

    :build-pathname "fb"
    :entry-point "fancybuild:main")


;#+sb-core-compression
;(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
;  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "fb/release"
    :depends-on (:fb)

    :entry-point "fancybuild:main"
    :build-pathname "fb"
    :build-operation "asdf:program-op")

(defsystem "fb/image"
    :depends-on (:fb)

    :build-pathname "fb"
    :build-operation "asdf:image-op")
