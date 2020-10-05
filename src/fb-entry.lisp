(in-package :fancybuild)

(export '(target main))

(defparameter *targets* (make-hash-table :test #'equal))
(defparameter *global-cache* (make-hash-table :test #'equal))

(defun target (name target)
  (setf (gethash name *targets*) target))

(defun naively-build (b)
  (mapcar #'naively-build (slot-value b 'dependencies))
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
             (uiop:quit 1))))
  (setf (slot-value b 'cached-snapshot) (snapshot b))
  (when (slot-value b 'unique-key)
    (setf (gethash (slot-value b 'unique-key) *global-cache*)
          (slot-value b 'cached-snapshot)))
  nil)

(defun main ()
  (unless (probe-file "fancy.build")
    (format t "Could not find build description file 'fancy.build'.~%")
    (uiop:quit 1))

  (when (probe-file "fancy.buildcache")
    (with-open-file (fp "fancy.buildcache"
                        :direction :input
                        :if-does-not-exist :error
                        :element-type '(unsigned-byte 8))
      (let ((buf (make-array (file-length fp) :element-type '(unsigned-byte 8))))
        (read-sequence buf fp)
        (setf *global-cache*
              (conspack:with-interning () (conspack:decode buf))))))

  (progn
    (in-package :fancily-built)
    (load "fancy.build"))

  (mapcar #'(lambda (target-name)
              (let ((target (gethash target-name *targets*)))
                (if target
                  (naively-build target)
                  (format t "No target named '~a'.~%" target-name))))
          (or (uiop:command-line-arguments) '("default")))

  (with-open-file (fp "fancy.buildcache"
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create
                      :element-type '(unsigned-byte 8))
    (write-sequence (conspack:encode *global-cache*) fp)))
