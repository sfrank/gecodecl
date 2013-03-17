;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

(defmacro with-gecode (&rest body)
  `(let ((*gspace* (make-gspace)))
     ,@body))

(defun one-solution (seq search-fun)
  (let ((s (funcall search-fun *gspace*)))
    (let ((solution (search-next s)))
      (when solution
        (let ((*gspace* solution))
          (map (type-of seq) #'integer-value seq))))))

(defun integer-seq (seq &key (min -1000000000) (max 1000000000) (result-type nil))
  (map (if result-type
           result-type
           (type-of seq))
       (lambda (x)
         (if (numberp x)
             x
             (add-int-variable min max)))
       seq))

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

;;; nvalues
(defgeneric nvalues-g (irt x y &key (clevel :icl-def)))

(defmethod nvalues-g (irt (x sequence) (y integer) &key (clevel :icl-def))
  (if (intvar-p (elt x 0))
      (gecode_nvalues_ivars_int *gspace* irt x y clevel)
      (gecode_nvalues_bvars_int *gspace* irt x y clevel)))

(defmethod nvalues-g (irt (x sequence) (y intvar) &key (clevel :icl-def))
  (if (intvar-p (elt x 0))
      (gecode_nvalues_ivars_int *gspace* irt x y clevel)
      (gecode_nvalues_bvars_int *gspace* irt x y clevel)))


;;; sequence
(defun sequence-g (seq set q l u &key (clevel :icl-def))
  (if (intvar-p (elt seq 0))
      (gecode_sequence_ivars *gspace* seq set q l u clevel)
      (gecode_sequence_bvars *gspace* seq set q l u clevel)))


;;; binpacking
(defun binpacking-g (loads bins sizes &key (clevel :icl-def))
  (gecode_binpacking *gspace* loads bins sizes clevel))


;;; nooverlap
(defun nooverlap-g (x w y h &key (o nil) (clevel :icl-def))
  (if o
      (gecode_nooverlap_optional *gspace* x w y h o clevel)
      (gecode_nooverlap *gspace* x w y h clevel)))

(defun nooverlap-coords-g (x0 w x1 y0 h y1 &key (o nil) (clevel :icl-def))
  (if o
      (gecode_nooverlap_coords_optional *gspace* x0 w x1 y0 h y1 o clevel)
      (gecode_nooverlap_coords *gspace* x0 w x1 y0 h y1 clevel)))



;;; extensional constraint

(defun extensional-g (seq dfa &key (clevel :icl-def))
  (if (intvar-p (elt seq 0))
      (gecode_extensional_ivars_dfa *gspace* seq dfa clevel)
      (gecode_extensional_bvars_dfa *gspace* seq dfa clevel)))

(defun extensional-tupleset-g (seq tuples &key (epk :epk-def) (clevel :icl-def))
  (if (intvar-p (elt seq 0))
      (gecode_extensional_ivars_tset *gspace* seq tuples epk clevel)
      (gecode_extensional_bvars_tset *gspace* seq tuples epk clevel)))

;;; translation/channelling

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


;;; branching

(defgeneric branchval-g (var val-brancher))

(defmethod branchval-g ((var intvar) (val-brancher ival-selector))
  (gecode_branch_ivar *gspace* var val-brancher))

(defmethod branchval-g ((var boolvar) (val-brancher ival-selector))
  (gecode_branch_bvar *gspace* var val-brancher))

(defmethod branchval-g ((var floatvar) (val-brancher fval-selector))
  (gecode_branch_fvar *gspace* var val-brancher))

(defmethod branchval-g ((var setvar) (val-brancher sval-selector))
  (gecode_branch_svar *gspace* var val-brancher))


(defgeneric branch-g (vars var-brancher val-brancher))

(defmethod branch-g ((vars sequence)
                     (var-brancher ivar-selector)
                     (val-brancher ival-selector))
  (cond
    ((every #'intvar-p vars)
     (gecode_branch_ivars *gspace* vars var-brancher val-brancher))
    ((every #'boolvar-p vars)
     (gecode_branch_bvars *gspace* vars var-brancher val-brancher))
    (t
     (error "Variable sequence VARS must contain a single type of variables of either type INTVAR or BOOLVAR."))))

(defmethod branch-g ((vars sequence)
                     (var-brancher fvar-selector)
                     (val-brancher fval-selector))
  (if (every #'floatvar-p vars)
      (gecode_branch_fvars *gspace* vars var-brancher val-brancher)
      (error "Variable sequence VARS must contain variables of type FLOATVAR.")))

(defmethod branch-g ((vars sequence)
                     (var-brancher svar-selector)
                     (val-brancher sval-selector))
  (if (every #'setvar-p vars)
      (gecode_branch_svars *gspace* vars var-brancher val-brancher)
      (error "Variable sequence VARS must contain variables of type SETVAR.")))

(defmethod branch-g ((vars sequence)
                     (var-brancher sequence)
                     (val-brancher ival-selector))
  (unless (every #'ivar-selector-p var-brancher)
    (error "Brancher sequence VAR-BRANCHER must contain variables of type IVAR-SELECTOR."))
  (let ((length (length var-brancher)))
    (cond 
      ((= length 1)
       (branch-g vars (elt var-brancher 0) val-brancher))
      ((<= 2 length 4)
       (cond
         ((every #'intvar-p vars)
          (branch-ivars-tie *gspace* vars var-brancher val-brancher))
         ((every #'boolvar-p vars)
          (branch-bvars-tie *gspace* vars var-brancher val-brancher))
         (t
          (error "Variable sequence VARS must contain a single type of variables of either type INTVAR or BOOLVAR."))))
      (t
       (error "The variable bancher sequence VAR-BRANCHER must not exceed four branchers.")))))

(defmethod branch-g ((vars sequence)
                     (var-brancher sequence)
                     (val-brancher fval-selector))
  (unless (every #'floatvar-p vars)
    (error "Variable sequence VARS must contain variables of type FLOATVAR."))
  (unless (every #'fvar-selector-p var-brancher)
    (error "Brancher sequence VAR-BRANCHER must contain variables of type FVAR-SELECTOR."))
  (let ((length (length var-brancher)))
    (cond 
      ((= length 1)
       (branch-g vars (elt var-brancher 0) val-brancher))
      ((<= 2 length 4)
       (branch-fvars-tie *gspace* vars var-brancher val-brancher))
      (t
       (error "The variable bancher sequence VAR-BRANCHER must not exceed four branchers.")))))

(defmethod branch-g ((vars sequence)
                     (var-brancher sequence)
                     (val-brancher sval-selector))
  (unless (every #'setvar-p vars)
    (error "Variable sequence VARS must contain variables of type SETVAR."))
  (unless (every #'svar-selector-p var-brancher)
    (error "Brancher sequence VAR-BRANCHER must contain variables of type SVAR-SELECTOR."))
  (let ((length (length var-brancher)))
    (cond 
      ((= length 1)
       (branch-g vars (elt var-brancher 0) val-brancher))
      ((<= 2 length 4)
       (branch-svars-tie *gspace* vars var-brancher val-brancher))
      (t
       (error "The variable bancher sequence VAR-BRANCHER must not exceed four branchers.")))))
