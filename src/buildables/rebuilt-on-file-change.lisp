(in-package :fancybuild)

(export '(rebuilt-on-file-change input-files output-files))

(defun mtime (x) (osicat-posix:stat-mtime (osicat-posix:stat x)))
; todo blake3?
(defun sha256 (fn)
  (with-open-file (fp fn
                      :direction :input
                      :if-does-not-exist :error
                      :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length fp) :element-type '(unsigned-byte 8))))
      (read-sequence buf fp)
      (ironclad:digest-sequence :sha256 buf))))

(defun any (pred &rest lis)
  (if (null (car lis))
    nil
    (or (apply pred (mapcar #'car lis))
        (apply #'any (cons pred (mapcar #'cdr lis))))))
(defun all (pred &rest lis)
  (if (null (car lis))
    t
    (and (apply pred (mapcar #'car lis))
         (apply #'all (cons pred (mapcar #'cdr lis))))))

(defun freeze-file-state (fpath)
  (cons (mtime fpath) (sha256 fpath)))

(defun has-file-changed (fpath state)
  (and (not (= (mtime fpath) (car state)))
       (not (equalp (sha256 fpath) (cdr state)))))

; Rebuild whenever any of the output files doesn't exist
; Or, whenever any of the input files has been modified
(defclass rebuilt-on-file-change (buildable)
  ((input-files :initarg :input-files :initform nil)
   (output-files :initarg :output-files :initform nil)))

(defmethod snapshot ((b rebuilt-on-file-change))
  (mapcar #'freeze-file-state (slot-value b 'input-files)))

(defmethod is-dirty ((b rebuilt-on-file-change) old)
  (or (not (any #'probe-file (slot-value b 'output-files)))
      (any #'has-file-changed (slot-value b 'input-files) old)))

