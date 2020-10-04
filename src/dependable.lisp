(defpackage #:fancybuild
  (:use #:cl)
  (:export #:main))

; to do the 'test' target:
;  - build the 'mocc' target
;  AND THEN
;  - run the file './aux/run-tests', failing iff it returns non-zero

; to do the 'mocc' target:
;  - build the 'libmocl.a' target
;  - build the 'src/buf.c' target
;  - build the 'src/tok.c' target
;  - build the 'src/parse.c' target
;  AND THEN
;  - run 'cc -o build/mocc build/cl/libmocl.a build/src/buf.o build/src/tok.o build/src/parse.o'
;
; ONLY IF 'mocc' needs to be rebuilt:

; to do the 'src/buf.c' target
;  - run 'cc -c -o build/src/buf.c.o src/buf.c'
; ONLY IF 'build/src/buf.c.o' needs to be rebuilt:
;  - build/src/buf.c.o doesn't exist
;  - build/src/buf.c.o exists, but src/buf.c has been modified since the last time we built build/src/buf.c.o
;  - build/src/buf.c.o exists, and src/buf.c hasn't been modified, but build/src/buf.c.d exists and one of the files in it has been modified since the last time we built build/src/buf.c.o



; Do I have to care about the relationship between 'src/buf.c' and 'build/src/buf.o'?  No


































; a buildable represents a relation from /dependencies/ to /products/
; a buildable will be built if any of its dependencies are dirty, or if it is dirty

; a buildable is dirty if its current state is incompatible with a previously snapshotted state
; or, if there is no previously snapshotted state--for instance, if this is the first time building the buildable

(in-package :fancybuild)

(defclass buildable ()
  ((dependencies :initarg :dependencies :initform nil)
   (cached-snapshot :initform nil)
   (pretty-name :initarg :pretty-name :initform "")
   (unique-key :initform nil)))

(defgeneric snapshot (buildable))
(defgeneric is-dirty (buildable old))
(defgeneric do-build (buildable))

(defun build (b)
  (mapcar #'build (slot-value b 'dependencies))
  (when (and (not (slot-value b 'cached-snapshot))
             (slot-value b 'unique-key)
             (gethash (slot-value b 'unique-key) *global-cache*))
    (setf (slot-value b 'cached-snapshot) (gethash (slot-value b 'unique-key) *global-cache*)))

  (unless (and (slot-value b 'cached-snapshot)
               (not (is-dirty b (slot-value b 'cached-snapshot))))
    (format t "Building ~a...~%" (slot-value b 'pretty-name))
    (handler-case (do-build b)
      (error ()
             (format t "Error building ~a!~%" (slot-value b 'pretty-name))
             (uiop:quit))))
  (setf (slot-value b 'cached-snapshot) (snapshot b))
  (when (slot-value b 'unique-key)
    (setf (gethash (slot-value b 'unique-key) *global-cache*)
          (slot-value b 'cached-snapshot)))
  nil)
