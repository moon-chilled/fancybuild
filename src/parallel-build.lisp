(in-package :fancybuild)

(defstruct parallel-buildable
  (base)
  (refcount 0)
  (dependents nil))

(defun construct-build-graph (b table)
  (let ((ret (if (gethash (unique-key b) table)
               (gethash (unique-key b) table)
               ; buildables are allowed to have nil keys, but replace with a dummy in that case
               ; because we need the value to be in the table
               (let ((key (or (unique-key b) (gensym))))
                 (setf (slot-value b 'cached-snapshot) (gethash key *global-cache*))
                 (setf (gethash key table) (make-parallel-buildable :base b))))))

    ; update dependencies, because construct-build-graph might run a deduplication on a dependency
    ; that being the case, if 'b' looks at one of its dependencies, it would see a stale dep--no bueno
    ; (this happens in practice with c-compiler and c-linker, which are depended on by 'most everything, but heavily dedup'd)
    ; so give it the new version
    (setf (dependencies b)
          (loop :for x :in (dependencies b)
                :collect (progn (incf (parallel-buildable-refcount ret))
                                (let ((newx (construct-build-graph x table)))
                                  (push ret (parallel-buildable-dependents newx))
                                  (parallel-buildable-base newx)))))
    ret))

(defun execute-parallel-build (table)
  (let ((currently-buildable '()))
    (flet ((unref (b)
                  (assert (> (parallel-buildable-refcount b) 0))
                  (when (= 0 (decf (parallel-buildable-refcount b)))
                    (push b currently-buildable))))
      (loop for v being the hash-value of table
            do (when (= (parallel-buildable-refcount v) 0) (push v currently-buildable)))
      (loop :while currently-buildable
            :do (let ((b (pop currently-buildable)))
                  (when (check-dirty (parallel-buildable-base b))
                    (awhen (pretty-name (parallel-buildable-base b))
                           (notice "building ~a..." -))
                    (do-build (parallel-buildable-base b))
                    (setf (slot-value (parallel-buildable-base b) 'cached-snapshot) (snapshot (parallel-buildable-base b)))
                    (setf (gethash (unique-key (parallel-buildable-base b)) *global-cache*) (slot-value (parallel-buildable-base b) 'cached-snapshot)))
                  (mapcar #'unref (parallel-buildable-dependents b)))))))

(defun parallel-build (buildable)
  (let ((table (make-hash-table :test #'equal)))
    (construct-build-graph buildable table)
    (execute-parallel-build table)))
