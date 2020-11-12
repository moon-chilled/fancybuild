(in-package :fancybuild)

(export '(target die dwarn notice))

(defparameter *targets* (make-hash-table :test #'equal))
(defparameter *global-cache* (make-hash-table :test #'equal))

(defmacro aif (test then &optional else)
  `(let ((- ,test))
     (if - ,then ,else)))
(defmacro awhen (test &body body)
  `(let ((- ,test))
     (when - ,@body)))

(defun stdout-tty-p () (interactive-stream-p *standard-output*))

(defun clr-maybe (seq)
  (if (stdout-tty-p)
    (apply #'format `(,nil ,seq #\esc)) 
    ""))
(defun clr-red () (clr-maybe "~a[31m"))
(defun clr-green () (clr-maybe "~a[32m"))
(defun clr-brown () (clr-maybe "~a[33m"))
(defun clr-blue () (clr-maybe "~a[34m"))
(defun clr-magenta () (clr-maybe "~a[35m"))
(defun clr-cyan () (clr-maybe "~a[36m"))
(defun clr-gray () (clr-maybe "~a[37m")) ; note: may be black on some terminals; avoid
(defun clr-reset () (clr-maybe "~a[0m"))
(defun clr-bold () (clr-maybe "~a[1m"))
(defun clr-dim () (clr-maybe "~a[2m"))
(defun clr-invert () (clr-maybe "~a[3m"))
(defun clr-underline () (clr-maybe "~a[4m"))

(defun die (&rest msg)
  (format t "~a~aERROR~a: ~a~%" (clr-bold) (clr-red) (clr-reset) (apply #'format (cons nil msg)))
  (uiop:quit 1))

(defun dwarn (&rest msg)
  (format t "~a~aWARNING~a: ~a~%" (clr-bold) (clr-brown) (clr-reset) (apply #'format (cons nil msg))))

(defun notice (&rest msg)
  (format t "~a~%" (apply #'format (cons nil msg))))

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
    (notice "building ~a..." (pretty-name b))
    (do-build b))
  (setf (slot-value b 'cached-snapshot) (snapshot b))
  (when (unique-key b)
    (setf (gethash (unique-key b) *global-cache*)
          (slot-value b 'cached-snapshot)))
  nil)

