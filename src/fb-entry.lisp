(in-package :fancybuild)

(export 'main)

(defun main ()
  (unless (probe-file "fancy.build")
    (die "Could not find build description file 'fancy.build'."))

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

  (let ((targets-list (mapcar #'(lambda (name)
                                  (or (gethash name *targets*) (die "No target named '~a'." name)))
                              (or (uiop:command-line-arguments) '("default")))))
    (parallel-build (if (cdr targets-list)
                      (make-instance 'buildable :dependencies targets-list)
                      (car targets-list))))
                  ;(naively-build target)


  (with-open-file (fp "fancy.buildcache"
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create
                      :element-type '(unsigned-byte 8))
    (write-sequence (conspack:encode *global-cache*) fp)))
