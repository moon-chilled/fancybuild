(in-package :fancybuild)

(defparameter *targets* (make-hash-table :test #'equal))
(defparameter *global-cache* (make-hash-table :test #'equal))

;  (let ((proc (uiop:launch-program "sleep 0.4")))
;    (format t "process is ~a.  Aliveness is ~a.  Waiting for it...~%" proc (uiop:process-alive-p proc))
;    (uiop:wait-process proc)
;    (format t "process is done!~%exiting~%"))

(defun main ()
  (unless (probe-file "fancy.build")
    (format t "Could not find build description file 'fancy.build'.~%")
    (uiop:quit))

  (when (probe-file "fancy.buildcache")
    (with-open-file (fp "fancy.buildcache"
                        :direction :input
                        :if-does-not-exist :error
                        :element-type '(unsigned-byte 8))
      (let ((buf (make-array (file-length fp) :element-type '(unsigned-byte 8))))
        (read-sequence buf fp)
        (setf *global-cache* (conspack:decode buf)))))

  (load "fancy.build")

  (mapcar #'(lambda (target-name)
              (let ((target (gethash target-name *targets*)))
                (if target
                  (build target)
                  (format t "No target named '~a'.~%" target-name))))
          (or (uiop:command-line-arguments) '("default")))

  (with-open-file (fp "fancy.buildcache"
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create
                      :element-type '(unsigned-byte 8))
    (write-sequence (conspack:encode *global-cache*) fp)))
