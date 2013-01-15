;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;; callbacks

;; The handler function for all C++ exceptions, standard and Gecode
;; specific ones. This was introduced to avoid the silent failing
;; should an exception happen in foreign code. We will now get a Lisp
;; side error with the exception error message.
(defcallback exception-fun :void ((msg :pointer))
  (error (concatenate 'string
                      "Caught C++ exception: "
                      (cffi:foreign-string-to-lisp msg))))

(defcallback update-int-var :void ((idx :unsigned-int) (mod modevent-int))
  ;;(format t "callback initiated~%")
  (funcall (aref (gspace-int-notifiers *gspace*) idx) mod))

(defun add-int-callback (variable function)
  (declare (type intvar variable))
  (let ((space *gspace*))
    (declare (type gspace space))
    (unless (gspace-int-notifiers space)
      (setf (gspace-int-notifiers space)
            (make-array 10 
                        :adjustable t
                        :fill-pointer 0)))
    (gecode_intClChannel space
                         (gvariable-index variable)
                         (vector-push-extend function
                                             (gspace-int-notifiers space)))))


(defun init-gecode ()
  (gecode_init_exceptionHandler (callback exception-fun))
  (gecode_init_callbackInt (callback update-int-var)))

(defvar *gecodecl-initialized-p* (progn
                                   (init-gecode)
                                   t))

;;; low level variable and space abstraction

(defstruct gvariable
  (index 0 :type (integer 0 #.most-positive-fixnum)))

(defstruct (intvar (:include gvariable)
                   (:constructor make-intvar (index))))

(defstruct (boolvar (:include intvar)
                    (:constructor make-boolvar (index))))

(defstruct (floatvar (:include gvariable)
                     (:constructor make-floatvar (index))))

(defstruct (gspace (:constructor %make-space)
                   (:constructor %make-space-boa (sap int-notifiers))
                   (:copier %copy-space))
  (sap (gecode_space_create) :type sb-sys:system-area-pointer :read-only t)
  (int-notifiers))

(defun reclaim-space (space)
  (lambda ()
    (format t "Space GCed...~%")
    (gecode_space_delete space)))

(defun make-gspace ()
  (let ((space (%make-space)))
    (tg:finalize space (reclaim-space space))
    space))

(defun make-gspace-from-ref (sap)
  (let ((space (%make-space-boa sap nil)))
    (tg:finalize space (reclaim-space space))
    space))

(defun copy-gspace (space)
  (declare (type gspace space))
  (let ((copy (%make-space-boa (gecode_space_copy space)
                               (gspace-int-notifiers space))))
    (tg:finalize space (reclaim-space copy))
    copy))


;;; IntVars
(defun add-int-variable ( &optional (min -1000000000) (max 1000000000))
  (make-intvar (gecode_int_addvar *gspace* min max)))

(defun intvar-set (set)
  (let ((iset (if (every #'integerp set)
                  (intset-seq set)
                  (intset-ranges set))))
    (make-intvar (gecode_int_addvar_set *gspace* iset))))

(defun integer-info (variable)
  (declare (type intvar variable))
  (with-foreign-objects ((min :int)
                         (max :int)
                         (size :int))
    (values (gecode_get_int_info *gspace*
                                 variable min max size)
            (list (mem-ref min :int)
                  (mem-ref max :int)
                  (mem-ref size :int)))))

(defun integer-value (variable)
  (multiple-value-bind (result values)
      (integer-info variable)
    (if (eq result :var-assigned)
        (car values)
        (error "Value requested on unassigned variable ~A" variable))))

;;; BoolVars
(defun add-bool-variable ()
  (make-boolvar (gecode_bool_addvar *gspace*)))

(defun boolean-info (variable)
  (declare (type boolvar variable))
  (with-foreign-object (value :int)
    ;(setf (mem-ref value :int) 0)
    (values (gecode_get_bool_info *gspace* variable value)
            (mem-ref value :int))))

(defun boolean-value (variable)
  (multiple-value-bind (result values)
      (boolean-info variable)
    (if (eq result :var-assigned)
        values
        (error "Value requested on unassigned variable ~A" variable))))

;;; FloatVars
(defun add-float-variable (&optional (min most-negative-double-float)
                                     (max most-positive-double-float))
  (make-floatvar (gecode_float_addvar *gspace* min max)))

(defun float-info (variable)
  (declare (type floatvar variable))
  (with-foreign-objects ((min :double)
                         (max :double)
                         (median :double))
    (values (gecode_get_float_info *gspace* variable min max median)
            (list (mem-ref min :double)
                  (mem-ref max :double)
                  (mem-ref median :double)))))

(defun float-value (variable)
  (multiple-value-bind (result values)
      (float-info variable)
    (if (eq result :var-assigned)
        (car values)
        (error "Value requested on unassigned variable ~A" variable))))


;;;; search engine interface and abstraction

(defstruct engine
  ;; sap will always be bound to an engine sap and never be
  ;; initialized to the default NIL.
  (sap nil :type sb-sys:system-area-pointer :read-only t))

(defstruct (dfs (:constructor %make-dfs-boa (sap))
                (:include engine)))

(defstruct (bab (:constructor %make-bab-boa (sap))
                (:include engine)))

;;; depth first search (DFS)
(defun reclaim-dfs (engine)
  (lambda ()
    (gecode_dfs_engine_delete engine)))

(defun make-dfs (space)
  (declare (type gspace space))
  (let ((dfs (%make-dfs-boa (gecode_dfs_engine_create space))))
    (tg:finalize dfs (reclaim-dfs dfs))
    dfs))

;;; branch and bound (BAB)
(defun reclaim-bab (engine)
  (lambda ()
    (gecode_bab_engine_delete engine)))

(defun make-bab (space min-var)
  (declare (type gspace space)
           (type intvar min-var))
  (let ((bab (%make-bab-boa (gecode_bab_engine_create space
                                                      (gvariable-index min-var)))))
    (tg:finalize bab (reclaim-bab bab))
    bab))

;;; solution search for both engine types
(defun search-next (engine)
  (etypecase engine
    (dfs (let ((space (gecode_dfs_engine_next engine)))
           (unless (null-pointer-p space)
             (make-gspace-from-ref space))))
    (bab (let ((space (gecode_bab_engine_next engine)))
           (unless (null-pointer-p space)
             (make-gspace-from-ref space))))))


;;; branching selectors
(defstruct selector
  (sap nil :type sb-sys:system-area-pointer :read-only t))
(defstruct (ivar-selector (:constructor %make-ivar-selector (sap))
                (:include selector)))
(defstruct (ival-selector (:constructor %make-ival-selector (sap))
                (:include selector)))
(defstruct (fvar-selector (:constructor %make-fvar-selector (sap))
                (:include selector)))
(defstruct (fval-selector (:constructor %make-fval-selector (sap))
                (:include selector)))


(defun reclaim-ivar-selector (selector)
  (lambda ()
    (gecode_ivar_selector_delete selector)))
(defun reclaim-ival-selector (selector)
  (lambda ()
    (gecode_ival_selector_delete selector)))

(defun make-ivar-selector (sap)
  (declare (type sb-sys:system-area-pointer sap))
  (let ((selector (%make-ivar-selector sap)))
    (tg:finalize selector (reclaim-ivar-selector selector))
    selector))
(defun make-ival-selector (sap)
  (declare (type sb-sys:system-area-pointer sap))
  (let ((selector (%make-ival-selector sap)))
    (tg:finalize selector (reclaim-ival-selector selector))
    selector))

(defun reclaim-fvar-selector (selector)
  (lambda ()
    (gecode_fvar_selector_delete selector)))
(defun reclaim-fval-selector (selector)
  (lambda ()
    (gecode_fval_selector_delete selector)))

(defun make-fvar-selector (sap)
  (declare (type sb-sys:system-area-pointer sap))
  (let ((selector (%make-fvar-selector sap)))
    (tg:finalize selector (reclaim-fvar-selector selector))
    selector))
(defun make-fval-selector (sap)
  (declare (type sb-sys:system-area-pointer sap))
  (let ((selector (%make-fval-selector sap)))
    (tg:finalize selector (reclaim-fval-selector selector))
    selector))

;;; sets

(defstruct (intset (:constructor %make-intset (sap)))
  (sap nil :type sb-sys:system-area-pointer :read-only t))

(defun reclaim-intset (set)
  (lambda ()
    (gecode_intset_delete set)))

(defun intset-bounds (min max)
  (let ((set (%make-intset (gecode_intset_bounds min max))))
    (tg:finalize set (reclaim-intset set))
    set))

(defun intset-seq (seq)
  (let ((length (length seq))
        (i -1))
    (with-foreign-object (array :int length)
      (map nil
           (lambda (x)
             (setf (mem-aref array :int (incf i)) x))
           seq)
      (let ((set (%make-intset (gecode_intset_seq array length))))
        (tg:finalize set (reclaim-intset set))
        set))))

(defun intset-ranges (array)
  (error "not yet implemented"))


;;; DFA

(defstruct (dfa (:constructor %make-dfa (sap)))
  (sap nil :type sb-sys:system-area-pointer :read-only t))

(defun DFA (start transitions finals)
  (let ((index -1))
    (declare (type fixnum index))
    (with-foreign-objects ((f :int (1+ (length finals)))
                           (trns '(:struct transition)
                                 (1+ (length transitions))))
      (map nil (lambda (x)
                 (declare (type fixnum x))
                 (setf (mem-aref f :int (incf index)) x))
           finals)
      (setf (mem-aref f :int (1+ index)) -1
            index 0)
      (map nil (lambda (x) 
                 (destructuring-bind (i s o) x
                   (declare (type fixnum i s o))
                   (with-foreign-slots ((i_state symbol o_state)
                                        (mem-aref trns '(:struct transition) (incf index))
                                        (:struct transition))
                     (setf i_state i
                           symbol s
                           o_state o))))
           (append transitions '((-1 0 0))))
      (let ((dfa (%make-dfa (gecode_DFA_create start trns f))))
        (tg:finalize dfa (lambda ()
                           (gecode_DFA_delete (dfa-sap dfa))))
        dfa))))


;;; Tuple-Set
(defstruct (tupleset (:constructor %make-tupleset (sap)))
  (sap nil :type sb-sys:system-area-pointer :read-only t))

(defun make-tupleset ()
  (let ((set (%make-tupleset (gecode_TupleSet_create))))
    (tg:finalize set (lambda ()
                       (gecode_TupleSet_delete set)))
    set))

(defun tupleset-add (set seq)
  (let ((length (length seq)))
    (declare (type (integer 0 #.most-positive-fixnum) length))
    (unless (zerop length)
      (gecode_TupleSet_add set seq))))

(defun tupleset-count (set)
  (gecode_TupleSet_count set))
