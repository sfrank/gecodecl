;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)


(defun test-1 ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable 1 4))
         (y (add-int-variable 3 3))
         (i (list 0 0 1))
         (l (list x y 4)))
    (declare (ignorable i))
    (gecode_distinct_ivars *gspace* l :icl-def)
    ;(gecode_distinct_ints_ivars *gspace* i l :icl-def)
    ;(distinct-g l)
    ;(distinct-offset-g i l :clevel :icl-def)
    (let ((dfs (make-dfs *gspace*)))
      (loop for s = (search-next dfs)
            until (null s)
            do (let ((*gspace* s))
                 (format t "X: ~A~%Y: ~A~%~%"
                         (integer-value x)
                         (integer-value y)))))))

(defun test-modification-callback ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable -1 4))
         (y (add-int-variable 3 4)))
    (add-int-callback x 
                      (lambda (m)
                        (format t "ADVISE: mod-event ~S for variable ~S~%"
                                m x)))
    (rel-post :irt-< x y)
    (integer-info x)))
