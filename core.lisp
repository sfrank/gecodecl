;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;; foreign libraries

(define-foreign-library gecode-kernel
  (t (:default "libgecodekernel")))

(define-foreign-library gecode-support
  (t (:default "libgecodesupport")))

(define-foreign-library gecode-int
  (t (:default "libgecodeint")))

(define-foreign-library libgmp
  (t (:default "libgmp")))
(define-foreign-library libmpfr
  (t (:default "libmpfr")))

(define-foreign-library gecode-float
  (t (:default "libgecodefloat")))

(define-foreign-library gecode-set
  (t (:default "libgecodeset")))

(define-foreign-library gecode-search
  (t (:default "libgecodesearch")))

(define-foreign-library gecode-minimodel
  (t (:default "libgecodeminimodel")))

(cffi:use-foreign-library gecode-kernel)
(cffi:use-foreign-library gecode-support)
(cffi:use-foreign-library gecode-int)
(cffi:use-foreign-library libgmp)
(cffi:use-foreign-library libmpfr)
(cffi:use-foreign-library gecode-float)
(cffi:use-foreign-library gecode-set)
(cffi:use-foreign-library gecode-search)
(cffi:use-foreign-library gecode-minimodel)


(define-foreign-library gecode-glue
  (t (:default "./lib/libgecodeglue")))
(cffi:use-foreign-library gecode-glue)


;;; callbacks

;; The handler function for all C++ exceptions, standard and Gecode
;; specific ones. This was introduced to void the silent failing
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
                                 (gvariable-index variable) min max size)
            (list (mem-ref min :int)
                  (mem-ref max :int)
                  (mem-ref size :int)))))

(defun integer-value (space variable)
  (multiple-value-bind (result values)
      (integer-info space variable)
    (if (eq result :var-assigned)
        (car values)
        (error "INTEGER-VALUE called on unassigned variable ~A" variable))))

;;; BoolVars
(defun add-bool-variable (space)
  (declare (type gspace space))
  (make-boolvar (gecode_bool_addvar space)))

(defun boolean-info (space variable)
  (declare   (type gspace space)
             (type boolvar variable))
  (with-foreign-object (value :int)
    (setf (mem-ref value :int) 0)
    (values (gecode_get_bool_info space (gvariable-index variable) value)
            (mem-ref value :int))))


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
    (values (gecode_get_float_info space
                                   (gvariable-index variable) min max median)
            (list (mem-ref min :double)
                  (mem-ref max :double)
                  (mem-ref median :double)))))

(defun float-value (space variable)
  (multiple-value-bind (result values)
      (float-info space variable)
    (if (eq result :var-assigned)
        (car values)
        (error "FLOAT-VALUE called on unassigned variable ~A" variable))))


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
(defun reclaim-dfs (sap)
  (lambda ()
    (gecode_dfs_engine_delete sap)))

(defun make-dfs (space)
  (declare (type gspace space))
  (let ((dfs (%make-dfs-boa (gecode_dfs_engine_create space))))
    (tg:finalize dfs (reclaim-dfs (dfs-sap dfs)))
    dfs))

;;; branch and bound (BAB)
(defun reclaim-bab (sap)
  (lambda ()
    (gecode_bab_engine_delete sap)))

(defun make-bab (space min-var)
  (declare (type gspace space)
           (type intvar min-var))
  (let ((bab (%make-bab-boa (gecode_bab_engine_create space
                                                      (gvariable-index min-var)))))
    (tg:finalize bab (reclaim-bab (bab-sap bab)))
    bab))

;;; solution search for both engine types
(defun search-next (engine)
  (etypecase engine
    (dfs (let ((space (gecode_dfs_engine_next (engine-sap engine))))
           (unless (null-pointer-p space)
             (make-gspace-from-ref space))))
    (bab (let ((space (gecode_bab_engine_next (engine-sap engine))))
           (unless (null-pointer-p space)
             (make-gspace-from-ref space))))))

#+ (or)
(defun test ()
  (let* ((*gspace* (make-gspace))
         (x (add-int-variable *gspace* 1 3))
         (y (add-int-variable *gspace* 3 3))
         dfs)
    (post-num-rel *gspace* :irt-< x y)
    (setf dfs (make-dfs *gspace*))
    (loop for s = (search-next dfs)
          until (null s)
          do (format t "X: ~A~%Y: ~A~%~%"
                     (integer-value s x)
                     (integer-value s y)))))

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
