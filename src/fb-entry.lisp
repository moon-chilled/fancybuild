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
  (load "fancy.build")
  (let ((target (gethash (or (cadr sb-ext:*posix-argv*) "default") *targets*)))
    (if target
      (progn
        (build target)
        (read-line *standard-input*)
        (build target))
      (format t "I don't know what to do with that...~%"))))
