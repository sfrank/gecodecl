;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;;; Boolean and Integer Relations

;;; type dispatch function to the different constraint propagator
;;; posting function which are normally dispatched by the C++
;;; compiletime polymorphism.

(defgeneric rel-post (rel x y b clevel space))

(defun switch-rel (rel)
  (ecase rel
    (:irt-<= :irt->=)
    (:irt-< :irt->)
    (:irt->= :irt-<=)
    (:irt-> :irt-<)
    (:irt-= :irt-=)
    (:irt-/= :irt-/=)))

(defmethod rel-post (rel (x boolvar) (y fixnum) (b null) clevel space)
  (check-type rel int-relation-type)
  (gcd-rel-bvar-int space
                    rel
                    (gvariable-index x)
                    y
                    clevel))

(defmethod rel-post (rel (x intvar) (y fixnum) (b null) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-ivar-int space
                    rel
                    (gvariable-index x)
                    y
                    clevel))

(defmethod rel-post (rel (x fixnum) (y boolvar) (b null) clevel space)
  (rel-post (switch-rel rel) y x b clevel space))

(defmethod rel-post (rel (x fixnum) (y intvar) (b null) clevel space)
  (rel-post (switch-rel rel) y x b clevel space))

(defmethod rel-post (rel (x boolvar) (y fixnum) (b boolvar) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-bvar-int-bvar space
                         rel
                         (gvariable-index x)
                         y
                         (gvariable-index b)
                         clevel))

(defmethod rel-post (rel (x intvar) (y fixnum) (b boolvar) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-ivar-int-bvar space
                         rel
                         (gvariable-index x)
                         y
                         (gvariable-index b)
                         clevel))

(defmethod rel-post (rel (x fixnum) (y boolvar) (b boolvar) clevel space)
  (rel-post (switch-rel rel) y x b clevel space))

(defmethod rel-post (rel (x fixnum) (y intvar) (b boolvar) clevel space)
  (rel-post (switch-rel rel) y x b clevel space))

(defmethod rel-post (rel (x boolvar) (y boolvar) (b null) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-bvar-bvar space
                     rel
                     (gvariable-index x)
                     (gvariable-index y)
                     clevel))

(defmethod rel-post (rel (x intvar) (y intvar) (b null) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-ivar-ivar space
                     rel
                     (gvariable-index x)
                     (gvariable-index y)
                     clevel))

(defmethod rel-post (rel (x boolvar) (y boolvar) (b boolvar) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-bvar-bvar-bvar space
                          rel
                          (gvariable-index x)
                          (gvariable-index y)
                          (gvariable-index b)
                          clevel))

(defmethod rel-post (rel (x intvar) (y intvar) (b boolvar) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (gcd-rel-ivar-ivar-bvar space
                          rel
                          (gvariable-index x)
                          (gvariable-index y)
                          (gvariable-index b)
                          clevel))

(defmethod rel-post (rel (x null) (y null) (b null) clevel space)
  (declare (ignore clevel space))
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->))))

(defmethod rel-post (rel (x sequence) (y null) (b null) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (let ((length (length x))
        (i 0))
    (if (intvar-p (first x))
        (if (every #'intvar-p x)
            (with-foreign-object (vars :int length)
              (map nil (lambda (v)
                         (setf (mem-aref vars :int i)
                               (gvariable-index v))
                         (incf i))
                   x)
              (gcd-rel-ivars space rel vars length clevel))
            (error "Every element of X must be of the same variable type INTVAR."))
        (if (every #'boolvar-p x)
            (with-foreign-object (vars :int length)
              (map nil (lambda (v)
                         (setf (mem-aref vars :int i)
                               (gvariable-index v))
                         (incf i))
                   x)
              (gcd-rel-bvars space rel vars length clevel))
            (error "Every element of X must be of the same variable type BOOLVAR.")))))

(defmethod rel-post (rel (x sequence) (y fixnum) (b null) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (let ((length (length x))
        (i 0))
    (if (intvar-p (first x))
        (if (every #'intvar-p x)
            (with-foreign-object (vars :int length)
              (map nil (lambda (v)
                         (setf (mem-aref vars :int i)
                               (gvariable-index v))
                         (incf i))
                   x)
              (gcd-rel-ivars-int space rel vars length y clevel))
            (error "Every element of X must be of the same variable type INTVAR."))
        (if (every #'boolvar-p x)
            (with-foreign-object (vars :int length)
              (map nil (lambda (v)
                         (setf (mem-aref vars :int i)
                               (gvariable-index v))
                         (incf i))
                   x)
              (gcd-rel-bvars-int space rel vars length y clevel))
            (error "Every element of X must be of the same variable type BOOLVAR.")))))

(defmethod rel-post (rel (x sequence) (y sequence) (b null) clevel space)
  (assert (member rel '(:irt-= :irt-/= :irt-<= :irt-< :irt->= :irt->)))
  (let ((xlength (length x))
        (ylength (length y))
        (i 0))
    (if (intvar-p (first x))
        (if (and (every #'intvar-p x)
                 (every #'intvar-p y))
            (with-foreign-objects ((xvars :int xlength)
                                   (yvars :int ylength))
              (map nil (lambda (v)
                         (setf (mem-aref xvars :int i)
                               (gvariable-index v))
                         (incf i))
                   x)
              (setf i 0)
              (map nil (lambda (v)
                         (setf (mem-aref yvars :int i)
                               (gvariable-index v))
                         (incf i))
                   y)
              (gcd-rel-ivars-ivars space
                                   rel xvars xlength yvars ylength clevel))
            (error "Every element of X and Y must be of the same variable type INTVAR."))
        (if (and (every #'boolvar-p x)
                 (every #'boolvar-p y))
            (with-foreign-objects ((xvars :int xlength)
                                   (yvars :int ylength))
              (map nil (lambda (v)
                         (setf (mem-aref xvars :int i)
                               (gvariable-index v))
                         (incf i))
                   x)
              (setf i 0)
              (map nil (lambda (v)
                         (setf (mem-aref yvars :int i)
                               (gvariable-index v))
                         (incf i))
                   y)
              (gcd-rel-bvars-bvars space
                                   rel xvars xlength yvars ylength clevel))
            (error "Every element of X and Y must be of the same variable type BOOLVAR.")))))


#+(or) ;; deprecated
(defun post-num-rel (space relation x y)
  (declare (type gspace space))
  (cond 
    ((and (typep x 'intvar)
          (typep y 'intvar))
     (gcd-rel-ivar-ivar space
                        relation (gvariable-index x) (gvariable-index y)))
    ((and (typep x 'boolvar)
          (typep y 'boolvar))
     (gcd-rel-bvar-bvar space
                        relation (gvariable-index x) (gvariable-index y)))
    (t
     (error "NUM-REL: X and Y must be of equal type of either INTVAR or BOOLVAR."))))

#+(or)
(defun test ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable *gspace* 1 3))
         (y (add-int-variable *gspace* 3 3)))
    (add-int-callback *gspace* x 
                      (lambda (m)
                        (format t "ADVISE: variable ~S changed with mod-event ~S.~%"
                                x m)))
    (post-num-rel *gspace* :irt-< x y)
    (integer-info *gspace* x)))

()

;;; Boolean operation and clauses

(defgeneric bop-post (bop x y b clevel space))

(defmethod bop-post (bop (x boolvar) (y boolvar) (b integer) clevel space)
  (check-type bop bool-operation-type)
  (check-type b (integer 0 1))
  (gcd-op-bvar-bvar-int space
                        bop
                        (gvariable-index x)
                        (gvariable-index y)
                        b
                        clevel))

(defmethod bop-post (bop (x boolvar) (y boolvar) (b boolvar) clevel space)
  (check-type bop bool-operation-type)
  (gcd-op-bvar-bvar-int space
                        bop
                        (gvariable-index x)
                        (gvariable-index y)
                        (gvariable-index b)
                        clevel))

(defmethod bop-post (bop (x sequence) (y integer) (b null) clevel space)
  (check-type bop bool-operation-type)
  (check-type y (integer 0 1))
  (let ((length (length x))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i length))
    (when (and (< length 2)
               (member bop '(:bot-imp :bot-eqv :bot-xor)))
      (error "Sequence length of argument X must be at least two for this operation."))
    (if (every #'boolvar-p x)
        (with-foreign-object (vars :int length)
          (map nil (lambda (v)
                     (setf (mem-aref vars :int i)
                           (gvariable-index v))
                     (incf i))
               x)
          (gcd-op-bvars-int space bop vars length y clevel))
        (error "Every element of X must be of the variable type BOOLVAR."))))

(defmethod bop-post (bop (x sequence) (y boolvar) (b null) clevel space)
  (check-type bop bool-operation-type)
  (let ((length (length x))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i length))
    (when (and (< length 2)
               (member bop '(:bot-imp :bot-eqv :bot-xor)))
      (error "Sequence length of argument X must be at least two for this operation."))
    (if (every #'boolvar-p x)
        (with-foreign-object (vars :int length)
          (map nil (lambda (v)
                     (setf (mem-aref vars :int i)
                           (gvariable-index v))
                     (incf i))
               x)
          (gcd-op-bvars-bvar space bop vars length (gvariable-index y) clevel))
        (error "Every element of X must be of the variable type BOOLVAR."))))

(defmethod bop-post (bop (x sequence) (y sequence) (b integer) clevel space)
  (check-type b (integer 0 1))
  (let ((xlength (length x))
        (ylength (length y))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i xlength ylength))
    (unless (member bop '(:bot-and :bot-or))
      (error "For these argument types the only defined operations are AND and OR."))
    (if (and (every #'boolvar-p x)
             (every #'boolvar-p y))
        (with-foreign-objects ((xvars :int xlength)
                               (yvars :int ylength))
          (map nil (lambda (v)
                     (setf (mem-aref xvars :int i)
                           (gvariable-index v))
                     (incf i))
               x)
          (setf i 0)
          (map nil (lambda (v)
                     (setf (mem-aref yvars :int i)
                           (gvariable-index v))
                     (incf i))
               y)
          (gcd-op-bvars-bvars-int space bop 
                                  xvars xlength
                                  yvars ylength 
                                  b clevel))
        (error "Every element of X and Y must be of the variable type BOOLVAR."))))

(defmethod bop-post (bop (x sequence) (y sequence) (b boolvar) clevel space)
  (let ((xlength (length x))
        (ylength (length y))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i xlength ylength))
    (unless (member bop '(:bot-and :bot-or))
      (error "For these argument types the only defined operations are AND and OR."))
    (if (and (every #'boolvar-p x)
             (every #'boolvar-p y))
        (with-foreign-objects ((xvars :int xlength)
                               (yvars :int ylength))
          (map nil (lambda (v)
                     (setf (mem-aref xvars :int i)
                           (gvariable-index v))
                     (incf i))
               x)
          (setf i 0)
          (map nil (lambda (v)
                     (setf (mem-aref yvars :int i)
                           (gvariable-index v))
                     (incf i))
               y)
          (gcd-op-bvars-bvars-bvar space bop 
                                   xvars xlength
                                   yvars ylength 
                                   (gvariable-index b) clevel))
        (error "Every element of X and Y must be of the variable type BOOLVAR."))))

;;; distinct integer variables

(defgeneric distinct-g (vseq cseq &key clevel space))

(defmethod distinct-g ((vseq null) (cseq null) &key clevel space)
  (declare (ignore clevel space)))

(defmethod distinct-g ((vseq sequence) (cseq null) &key clevel space)
  (let ((clevel (if clevel clevel :icl-def))
        (space (if space space *gspace*))
        (vlength (length vseq))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i vlength))
    (if (every #'intvar-p vseq)
        (with-foreign-object (vars :int vlength)
          (map nil (lambda (x)
                     (setf (mem-aref vars :int i)
                           (gvariable-index x))
                     (incf i))
               vseq)
          (gecode_distinct_ivars space vars vlength clevel))
        (error "Every element of VSEQ must be of type INTVAR."))))

(defmethod distinct-g ((vseq sequence) (cseq sequence) &key clevel space)
  (let ((clevel (if clevel clevel :icl-def))
        (space (if space space *gspace*))
        (vlength (length vseq))
        (clength (length cseq))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i vlength clength))
    (when (/= vlength clength)
      (error "VSEQ and CSEQ must be of equal length."))
    (if (every #'intvar-p vseq)
        (if (every #'(lambda (x) (typep x 'fixnum)) cseq)
            (with-foreign-objects ((vars :int vlength)
                                   (offsets :int vlength))
              (map nil (lambda (x y)
                         (setf (mem-aref vars :int i)
                               (gvariable-index x))
                         (setf (mem-aref offsets :int i) y)
                         (incf i))
                   vseq cseq)
              (gecode_distinct_ints_ivars
               space offsets vars vlength clevel))
            (error "Every element of CSEQ must be of type FIXNUM."))
        (error "Every element of VSEQ must be of type INTVAR."))))

#+ (or)
(defun distinct-g (vseq &key (cseq nil) (clevel :icl-def) (space *gspace*))
  (declare (type gspace space))
  (unless vseq
    (return-from distinct-g))
  (let ((vlength (length vseq))
        (i 0))
    (if (every #'intvar-p vseq)
        (cond
          (cseq
           (when (/= vlength (length cseq))
             (error "VSEQ and CSEQ must be of equal length."))
           (if (every #'(lambda (x) (typep x 'fixnum)) cseq)
               (with-foreign-objects ((vars :int vlength)
                                      (offsets :int vlength))
                 (map nil (lambda (x y)
                            (setf (mem-aref vars :int i)
                                  (gvariable-index x))
                            (setf (mem-aref offsets :int i) y)
                            (incf i))
                      vseq cseq)
                 (gcd-distinct-ints-intvars
                  space offsets vars vlength clevel))
               (error "Every element of CSEQ must be of type FIXNUM.")))
          (t
           (with-foreign-object (vars :int vlength)
             (map nil (lambda (x)
                        (setf (mem-aref vars :int i)
                              (gvariable-index x))
                        (incf i))
                  vseq)
             (gcd-distinct-intvars space vars vlength clevel))))
        (error "Every element of VSEQ must be of type INTVAR."))))

#+ (or)
(defun test ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable *gspace* 1 3))
         (y (add-int-variable *gspace* 3 3))
         dfs)
    (distinct-g (list x y) nil)
    (setf dfs (make-dfs *gspace*))
    (loop for s = (search-next dfs)
          until (null s)
          do (format t "X: ~A~%Y: ~A~%~%"
                     (integer-value s x)
                     (integer-value s y)))))

(defgeneric sorted-g (xseq yseq zseq &key clevel space))

(defmethod sorted-g ((xseq sequence) (yseq sequence) (zseq null) &key clevel space)
  (let ((clevel (if clevel clevel :icl-def))
        (space (if space space *gspace*))
        (length (length xseq))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i length))
    (when (<= length 1)
      (return-from sorted-g))
    (if (and (every #'intvar-p xseq)
             (every #'intvar-p yseq))
        (if (= length (length yseq))
            (with-foreign-objects ((xa :int length)
                                   (ya :int length))
              (map nil (lambda (x y)
                         (setf (mem-aref xa :int i)
                               (gvariable-index x)
                               (mem-aref ya :int i)
                               (gvariable-index y))
                         (incf i))
                   xseq yseq)
              (gcd-sorted-intvars-intvars space xa ya length clevel))
            (error "XSEQ and YSEQ must be of equal length."))
        (error "Every elements of XSEQ and YSEQ must be of type INTVAR."))))

(defmethod sorted-g ((xseq sequence) (yseq sequence) (zseq sequence) &key clevel space)
  (let ((clevel (if clevel clevel :icl-def))
        (space (if space space *gspace*))
        (length (length xseq))
        (i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i length))
    (when (<= length 1)
      (return-from sorted-g))
    (if (and (every #'intvar-p xseq)
             (every #'intvar-p yseq)
             (every #'intvar-p zseq))
        (if (= length (length yseq) (length zseq))
            (with-foreign-objects ((xa :int length)
                                   (ya :int length)
                                   (za :int length))
              (map nil (lambda (x y z)
                         (setf (mem-aref xa :int i)
                               (gvariable-index x)
                               (mem-aref ya :int i)
                               (gvariable-index y)
                               (mem-aref za :int i)
                               (gvariable-index z))
                         (incf i))
                   xseq yseq zseq)
              (gcd-sorted-intvars-intvars-intvars space
                                                  xa ya za length clevel))
            (error "XSEQ, YSEQ and ZSEQ must be of equal length."))
        (error "Every elements of XSEQ, YSEQ, ZSEQ must be of type INTVAR."))))

