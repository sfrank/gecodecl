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

(defun add-int-callback (space variable function)
  (declare (type gspace space)
           (type intvar variable))
  (unless (gspace-int-notifiers space)
    (setf (gspace-int-notifiers space)
          (make-array 10 
                      :adjustable t
                      :fill-pointer 0)))
  (gecode_intClChannel space
                       (gvariable-index variable)
                       (vector-push-extend function
                                           (gspace-int-notifiers space))))


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
(defun add-int-variable (space &optional (min -1000000000) (max 1000000000))
  (declare (type gspace space))
  (make-intvar (gecode_int_addvar space min max)))

(defun integer-info (space variable)
  (declare (type gspace space)
           (type intvar variable))
  (with-foreign-objects ((min :int)
                         (max :int)
                         (size :int))
    (values (gecode_get_int_info space
                                 variable min max size)
            (list (mem-ref min :int)
                  (mem-ref max :int)
                  (mem-ref size :int)))))

(defun integer-value (space variable)
  (multiple-value-bind (result values)
      (integer-info space variable)
    (if (eq result :var-assigned)
        (car values)
        (error "Value requested on unassigned variable ~A" variable))))

;;; BoolVars
(defun add-bool-variable (space)
  (declare (type gspace space))
  (make-boolvar (gecode_bool_addvar space)))

(defun boolean-info (space variable)
  (declare   (type gspace space)
             (type boolvar variable))
  (with-foreign-object (value :int)
    ;(setf (mem-ref value :int) 0)
    (values (gecode_get_bool_info space variable value)
            (mem-ref value :int))))

(defun boolean-value (space variable)
  (multiple-value-bind (result values)
      (boolean-info space variable)
    (if (eq result :var-assigned)
        values
        (error "Value requested on unassigned variable ~A" variable))))

;;; FloatVars
(defun add-float-variable (space &optional (min most-negative-double-float)
                                           (max most-positive-double-float))
  (declare (type gspace space))
  (make-floatvar (gecode_float_addvar space min max)))

(defun float-info (space variable)
  (declare (type gspace space)
           (type floatvar variable))
  (with-foreign-objects ((min :double)
                         (max :double)
                         (median :double))
    (values (gecode_get_float_info space variable min max median)
            (list (mem-ref min :double)
                  (mem-ref max :double)
                  (mem-ref median :double)))))

(defun float-value (space variable)
  (multiple-value-bind (result values)
      (float-info space variable)
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

#+ (or)
(defun test ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable *gspace* 1 4))
         (y (add-int-variable *gspace* 3 3))
         (i (list 0 1))
         (l (list x y))
         dfs)
    ;(post-num-rel *gspace* :irt-< x y)
    ;(gecode_distinct_ivars *gspace* l :icl-def)
    (gecode_distinct_ints_ivars *gspace* i l :icl-def)
    ;(distinct-g l nil)
    (setf dfs (make-dfs *gspace*))
    (loop for s = (search-next dfs)
          until (null s)
          do (let ((*gspace* s))
               (format t "X: ~A~%Y: ~A~%~%"
                       (integer-value s x)
                       (integer-value s y))))))

#+ (or)
(defun test-bab ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable *gspace* 1 3))
         (y (add-int-variable *gspace* 3 3)))
    (post-num-rel *gspace* :irt-< x y)
    (let ((s (search-next (make-bab *gspace* x))))
      (when s
        (format t "X: ~A~%Y: ~A~%"
                     (integer-value s x)
                     (integer-value s y))))))
