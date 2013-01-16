;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;;; Boolean and Integer Relations

;;; type dispatch function to the different constraint propagator
;;; posting function which are normally dispatched by the C++
;;; compiletime polymorphism.

(defgeneric rel-post (rel x y &key clevel))
(defgeneric rel-post-reified (rel x y mode b &key clevel))

(defun switch-rel (rel)
  (ecase rel
    (:irt-<= :irt->=)
    (:irt-< :irt->)
    (:irt->= :irt-<=)
    (:irt-> :irt-<)
    (:irt-= :irt-=)
    (:irt-/= :irt-/=)))

(defmethod rel-post (rel (x boolvar) (y integer) &key (clevel :icl-def))
  (gecode_rel_bvar_int *gspace* rel x y clevel))

(defmethod rel-post (rel (x intvar) (y integer) &key (clevel :icl-def))
  (gecode_rel_ivar_int *gspace* rel x y clevel))

(defmethod rel-post (rel (x integer) (y boolvar) &key (clevel :icl-def))
  (gecode_rel_bvar_int *gspace* (switch-rel rel) y x clevel))

(defmethod rel-post (rel (x integer) (y intvar) &key (clevel :icl-def))
  (gecode_rel_ivar_int *gspace* (switch-rel rel) y x clevel))

(defmethod rel-post-reified (rel (x boolvar) (y integer) 
                             mode (b boolvar) &key (clevel :icl-def))
  (gecode_rel_bvar_int_reified *gspace* rel x y mode b clevel))

(defmethod rel-post-reified (rel (x intvar) (y integer)
                             mode (b boolvar) &key (clevel :icl-def))
  (gecode_rel_ivar_int_reified *gspace* rel x y mode b clevel))

(defmethod rel-post-reified (rel (x integer) (y boolvar)
                             mode (b boolvar) &key (clevel :icl-def))
  (gecode_rel_bvar_int_reified *gspace* (switch-rel rel) y x mode b clevel))

(defmethod rel-post-reified (rel (x fixnum) (y intvar)
                             mode (b boolvar) &key (clevel :icl-def))
  (gecode_rel_ivar_int_reified *gspace* (switch-rel rel) y x mode b clevel))

(defmethod rel-post (rel (x boolvar) (y boolvar) &key (clevel :icl-def))
  (gecode_rel_bvar_bvar *gspace* rel x y clevel))

(defmethod rel-post (rel (x intvar) (y intvar) &key (clevel :icl-def))
  (gecode_rel_ivar_ivar *gspace* rel x y clevel))

(defmethod rel-post-reified (rel (x boolvar) (y boolvar) 
                             mode (b boolvar) &key (clevel :icl-def))
  (gecode_rel_bvar_bvar_reified *gspace* rel x y mode b clevel))

(defmethod rel-post-reified (rel (x intvar) (y intvar)
                             mode (b boolvar) &key (clevel :icl-def))
  (gecode_rel_ivar_ivar_reified *gspace* rel x y mode b clevel))

(defmethod rel-post (rel (x sequence) (y null) &key (clevel :icl-def))
  (declare (ignore y))
  (cond
    ((or (find-if #'intvar-p x)
         (every #'integerp x))
     (gecode_rel_ivars *gspace* rel x clevel))
    ((find-if #'boolvar-p x)
     (gecode_rel_bvars *gspace* rel x clevel))
    (t
     (error "Could not determine applicable Gecode method for input sequence X."))))

(defmethod rel-post (rel (x sequence) (y integer) &key (clevel :icl-def))
  (cond
    ((or (find-if #'intvar-p x)
         (every #'integerp x))
     (gecode_rel_ivars_int *gspace* rel x y clevel))
    ((find-if #'boolvar-p x)
     (gecode_rel_bvars_int *gspace* rel x y clevel))
    (t
     (error "Could not determine applicable Gecode method for input sequence X."))))

(defmethod rel-post (rel (x sequence) (y sequence) &key (clevel :icl-def))
  (cond
    ((and (or (find-if #'intvar-p x)
              (every #'integerp x))
          (or (find-if #'intvar-p y)
              (every #'integerp y)))
     (gecode_rel_ivars_ivars *gspace* rel x y clevel))
    ((and (find-if #'boolvar-p x)
          (find-if #'boolvar-p y))
     (gecode_rel_bvars_bvars *gspace* rel x y clevel))
    (t
     (error "Could not determine applicable Gecode method for input sequence X."))))


;;; Boolean operation and clauses

(defgeneric bop-post (bop x y b &key clevel))

(defmethod bop-post (bop (x boolvar) (y boolvar) (b boolvar) &key (clevel :icl-def))
  (gecode_op_bvar_bvar_bvar *gspace* bop x y b clevel))

(defmethod bop-post (bop (x boolvar) (y boolvar) (b integer) &key (clevel :icl-def))
  (gecode_op_bvar_bvar_int *gspace* bop x y b clevel))

(defgeneric bop-seq-post (bop x b &key clevel))

(defmethod bop-seq-post (bop (x sequence) (b boolvar) &key (clevel :icl-def))
  (gecode_op_bvars_bvar *gspace* bop x b clevel))

(defmethod bop-seq-post (bop (x sequence) (b integer) &key (clevel :icl-def))
  (gecode_op_bvars_int *gspace* bop x b clevel))

(defgeneric bop-clause-post (bop x y b &key clevel))

(defmethod bop-clause-post (bop (x sequence) (y sequence) (b integer) &key (clevel :icl-def))
  (unless (member bop '(:bot-and :bot-or))
    (error "Clauses are only defined for AND and OR."))
  (gecode_clause_bvars_bvars_int *gspace* bop x y b clevel))

(defmethod bop-clause-post (bop (x sequence) (y sequence) (b boolvar) &key (clevel :icl-def))
  (unless (member bop '(:bot-and :bot-or))
    (error "Clauses are only defined for AND and OR."))
  (gecode_clause_bvars_bvars_bvar *gspace* bop x y b clevel))


;;; distinct integer variables

(defun distinct-g (varseq &key (clevel :icl-def))
  (let ((length (length varseq)))
    (declare (type (integer 0 #.most-positive-fixnum) length))
    (unless (zerop length)
      (gecode_distinct_ivars *gspace* varseq clevel))))

(defun distinct-offset-g (offsetseq varseq &key (clevel :icl-def))
  (let ((vlength (length varseq))
        (olength (length offsetseq)))
    (declare (type (integer 0 #.most-positive-fixnum) vlength olength))
    (if (/= vlength olength)
        (error "VARSEQ and OFFSETSEQ must be of equal length.")
        (unless (zerop vlength)
          (gecode_distinct_ints_ivars *gspace* offsetseq varseq clevel)))))

;;; sortedness of integer variables

(defun sorted-g (xseq yseq &key (clevel :icl-def))
  (let ((xlength (length xseq))
        (ylength (length yseq)))
    (declare (type (integer 0 #.most-positive-fixnum) xlength ylength))
    (if (/= xlength ylength)
        (error "XSEQ and YSEQ must be of equal length.")
        (unless (zerop xlength)
          (gecode_sorted_ivars_ivars *gspace* xseq yseq clevel)))))

(defun sorted-permutation-g (xseq yseq permseq &key (clevel :icl-def))
  (let ((xlength (length xseq))
        (ylength (length yseq))
        (plength (length permseq)))
    (declare (type (integer 0 #.most-positive-fixnum) xlength ylength plength))
    (if (/= xlength ylength plength)
        (error "XSEQ, YSEQ and PERMSEQ must be of equal length.")
        (unless (zerop xlength)
          (gecode_sorted_ivars_ivars_ivars *gspace* xseq yseq permseq clevel)))))


;;; sequence
(defun sequence-g (seq set q l u &key (clevel :icl-def))
  (if (intvar-p (elt seq 0))
      (gecode_sequence_ivars *gspace* seq set q l u clevel)
      (gecode_sequence_bvars *gspace* seq set q l u clevel)))


;;; binpacking

(defun binpacking-g (loads bins sizes &key (clevel :icl-def))
  (gecode_binpacking *gspace* loads bins sizes clevel))


;;; extensional constraint

(defun extensional-g (seq dfa &key (clevel :icl-def))
  (if (intvar-p (elt seq 0))
      (gecode_extensional_ivars_dfa *gspace* seq dfa clevel)
      (gecode_extensional_bvars_dfa *gspace* seq dfa clevel)))

(defun extensional-tupleset-g (seq tuples &key (epk :epk-def) (clevel :icl-def))
  (if (intvar-p (elt seq 0))
      (gecode_extensional_ivars_tset *gspace* seq tuples epk clevel)
      (gecode_extensional_bvars_tset *gspace* seq tuples epk clevel)))

;;; translation/channeling

(defgeneric channel-g (x y &key clevel))

(defmethod channel-g ((x intvar) (y boolvar) &key (clevel :icl-def))
  (gecode_channel_ivar_bvar *gspace* x y clevel))

(defmethod channel-g ((x boolvar) (y intvar) &key (clevel :icl-def))
  (gecode_channel_bvar_ivar *gspace* x y clevel))

(defmethod channel-g ((x floatvar) (y intvar) &key (clevel :icl-def))
  (declare (ignore clevel))
  (gecode_channel_fvar_ivar *gspace* x y))

(defmethod channel-g ((x intvar) (y floatvar) &key (clevel :icl-def))
  (declare (ignore clevel))
  (gecode_channel_fvar_ivar *gspace* y x))

(defun channels-offset-g (x y &key (xoffset 0) (yoffset 0) (clevel :icl-def))
  (if (= 0 xoffset yoffset)
      (gecode_channel_ivars_ivars *gspace* x y clevel)
      (gecode_channel_ivars_int_ivars_int *gspace* x xoffset y yoffset clevel)))

(defun channel-bools-g (seq integer &key (offset 0) (clevel :icl-def))
  (gecode_channel_bvars_ivar_int *gspace* seq integer offset clevel))
