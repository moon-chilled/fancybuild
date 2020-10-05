(in-package :fancybuild)

(export 'target)

(defparameter *targets* (make-hash-table :test #'equal))
(defparameter *global-cache* (make-hash-table :test #'equal))

(defmacro aif (test then &optional else)
  `(let ((- ,test))
     (if - ,then ,else)))
(defmacro awhen (test &body body)
  `(let ((- ,test))
     (when - ,@body)))
(defmacro aunless (test &body body)
  `(let ((- ,test))
     (unless - ,@body)))

(defun die (&rest msg)
  (apply #'format (cons t msg))
  (format t "~%")
  (uiop:quit 1))

(defun target (name target)
  (setf (gethash name *targets*) target))

(defun naively-build (b)
  (mapcar #'naively-build (dependencies b))
  (when (and (not (slot-value b 'cached-snapshot))
             (unique-key b)
             (gethash (unique-key b) *global-cache*))
    (setf (slot-value b 'cached-snapshot) (gethash (unique-key b) *global-cache*)))

  (unless (and (slot-value b 'cached-snapshot)
               (not (is-dirty b (slot-value b 'cached-snapshot))))
    (format t "Building ~a...~%" (pretty-name b))
    (handler-case (do-build b)
      (error ()
             (die "Error building ~a!" (pretty-name b)))))
  (setf (slot-value b 'cached-snapshot) (snapshot b))
  (when (unique-key b)
    (setf (gethash (unique-key b) *global-cache*)
          (slot-value b 'cached-snapshot)))
  nil)

