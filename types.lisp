;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)


(defun elements-are-of-type (seq type)
  (every #'(lambda (x) (typep x type)) seq))

(deftype seq-of-type (type)
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (seq) (elements-are-of-type seq type)) )
    `(and sequence (satisfies ,predicate)) ))

;(typep '(1 2 3) '(seq-of-type integer))
;; -> T

(deftype int-relation-type ()
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (rel) (member rel '(:irt-=
                                    :irt-/=
                                    :irt-<=
                                    :irt-<
                                    :irt->=
                                    :irt->)
                              :test #'eq)) )
    `(and keyword (satisfies ,predicate))))

(deftype bool-operation-type ()
  (let ((operation (gensym)))
    (setf (symbol-function operation)
      #'(lambda (op) (member op '(:bot-and
                                  :bot-or
                                  :bot-imp
                                  :bot-eqv
                                  :bot-xor)
                             :test #'eq)) )
    `(and keyword (satisfies ,operation))))

(defparameter *gspace* nil)


;;; CFFI type translators

;; space
(cffi:define-foreign-type space-type () ()
  (:actual-type :pointer)
  (:simple-parser space-type))

(defmethod expand-to-foreign (space (type space-type))
  `(gspace-sap ,space))

;; search 
(cffi:define-foreign-type search-type () ()
  (:actual-type :pointer)
  (:simple-parser search-type))

(defmethod expand-to-foreign (engine (type search-type))
  `(engine-sap ,engine))

;; variables
(cffi:define-foreign-type intvar-type () ()
  (:actual-type :pointer)
  (:simple-parser intvar-type))

(defmethod expand-to-foreign (intvar (type intvar-type))
  `(gecode_get_intvar_by_index *gspace* (intvar-index ,intvar)))

(cffi:define-foreign-type boolvar-type () ()
  (:actual-type :pointer)
  (:simple-parser boolvar-type))

(defmethod expand-to-foreign (boolvar (type boolvar-type))
  `(gecode_get_boolvar_by_index *gspace* (boolvar-index ,boolvar)))

(cffi:define-foreign-type floatvar-type () ()
  (:actual-type :pointer)
  (:simple-parser floatvar-type))

(defmethod expand-to-foreign (floatvar (type floatvar-type))
  `(gecode_get_floatvar_by_index *gspace* (floatvar-index ,floatvar)))

(cffi:define-foreign-type setvar-type () ()
  (:actual-type :pointer)
  (:simple-parser setvar-type))

(defmethod expand-to-foreign (setvar (type setvar-type))
  `(gecode_get_setvar_by_index *gspace* (setvar-index ,setvar)))

;; brancher handles
(cffi:define-foreign-type brancherhandle-type () ()
  (:actual-type :pointer)
  (:simple-parser brancherhandle-type))

(defmethod expand-to-foreign (handle (type brancherhandle-type))
  `(brancher-handle-sap ,handle))
(defmethod expand-from-foreign (sap (type brancherhandle-type))
  `(make-brancher-handle ,sap))


;; branchers
(cffi:define-foreign-type intvarselector-type () ()
  (:actual-type :pointer)
  (:simple-parser intvarselector-type))

(defmethod expand-to-foreign (selector (type intvarselector-type))
  `(selector-sap ,selector))
(defmethod expand-from-foreign (sap (type intvarselector-type))
  `(make-ivar-selector ,sap))

(cffi:define-foreign-type intvalselector-type () ()
  (:actual-type :pointer)
  (:simple-parser intvalselector-type))

(defmethod expand-to-foreign (selector (type intvalselector-type))
  `(selector-sap ,selector))
(defmethod expand-from-foreign (sap (type intvalselector-type))
  `(make-ival-selector ,sap))

(cffi:define-foreign-type floatvarselector-type () ()
  (:actual-type :pointer)
  (:simple-parser floatvarselector-type))

(defmethod expand-to-foreign (selector (type floatvarselector-type))
  `(selector-sap ,selector))
(defmethod expand-from-foreign (sap (type floatvarselector-type))
  `(make-fvar-selector ,sap))

(cffi:define-foreign-type floatvalselector-type () ()
  (:actual-type :pointer)
  (:simple-parser floatvalselector-type))

(defmethod expand-to-foreign (selector (type floatvalselector-type))
  `(selector-sap ,selector))
(defmethod expand-from-foreign (sap (type floatvalselector-type))
  `(make-fval-selector ,sap))


;; argument vectors of various types
(cffi:define-foreign-type intvarargs-type () ()
  (:actual-type :pointer)
  (:simple-parser intvarargs-type))

(defmethod expand-to-foreign-dyn (value var body (type intvarargs-type))
  (let ((length (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_varargs_create ,length)) ; the argument array class
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (let ((x (if (integerp x) ; if there is an integer coerce to variable
                           (add-int-variable x x)
                           x)))
                (declare (type intvar x))
                (gecode_varargs_set ,var (incf ,i) x)))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_varargs_delete ,var)))))

(cffi:define-foreign-type intargs-type () ()
  (:actual-type :pointer)
  (:simple-parser intargs-type))

(defmethod expand-to-foreign-dyn (value var body (type intargs-type))
  (let ((length (gensym))
        (adr (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_intargs_create ,length))
            (,adr (gecode_intargs_adr ,var))
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (declare (type fixnum x))
              (setf (mem-aref ,adr :int (incf ,i)) x))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_intargs_delete ,var)))))

(cffi:define-foreign-type boolvarargs-type () ()
  (:actual-type :pointer)
  (:simple-parser boolvarargs-type))

(defmethod expand-to-foreign-dyn (value var body (type boolvarargs-type))
  (let ((length (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_varargs_create ,length))
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (declare (type boolvar x))
              (gecode_varargs_set ,var (incf ,i) x))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_varargs_delete ,var)))))

(cffi:define-foreign-type floatvarargs-type () ()
  (:actual-type :pointer)
  (:simple-parser floatvarargs-type))

(defmethod expand-to-foreign-dyn (value var body (type floatvarargs-type))
  (let ((length (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_varargs_create ,length))
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (declare (type floatvar x))
              (gecode_varargs_set ,var (incf ,i) x))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_varargs_delete ,var)))))

(cffi:define-foreign-type floatargs-type () ()
  (:actual-type :pointer)
  (:simple-parser floatargs-type))

(defmethod expand-to-foreign-dyn (value var body (type floatargs-type))
  (let ((length (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_floatargs_create ,length)) ; the argument array class
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (gecode_floatargs_set ,var (incf ,i) x))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_floatargs_delete ,var)))))

(cffi:define-foreign-type intsetargs-type () ()
  (:actual-type :pointer)
  (:simple-parser intsetargs-type))

(defmethod expand-to-foreign-dyn (value var body (type intsetargs-type))
  (let ((length (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_intsetargs_create ,length))
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (declare (type intset x))
              (gecode_intsetargs_set ,var (incf ,i) x))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_intsetargs_delete ,var)))))


(cffi:define-foreign-type tasktypeargs-type () ()
  (:actual-type :pointer)
  (:simple-parser tasktypeargs-type))

(defmethod expand-to-foreign-dyn (value var body (type tasktypeargs-type))
  (let ((length (gensym))
        (adr (gensym))
        (i (gensym)))
    `(let* ((,length (length ,value))
            (,var (gecode_intargs_create ,length))
            (,adr (gecode_intargs_adr ,var))
            (,i -1))
       (declare (type fixnum ,i ,length))
       (map nil
            (lambda (x)
              (declare (type fixnum x))
              (setf (mem-aref ,adr :int (incf ,i))
                    (foreign-enum-value 'task-type x)))
            ,value)
       (unwind-protect 
            (progn ,@body)
         (gecode_intargs_delete ,var)))))


(cffi:define-foreign-type intset-type () ()
  (:actual-type :pointer)
  (:simple-parser intset-type))

(defmethod expand-to-foreign (set (type intset-type))
  `(intset-sap ,set))


(cffi:define-foreign-type dfa-type () ()
  (:actual-type :pointer)
  (:simple-parser dfa-type))

(cffi:define-foreign-type tupleset-type () ()
  (:actual-type :pointer)
  (:simple-parser tupleset-type))

(defmethod expand-to-foreign (set (type tupleset-type))
  `(tupleset-sap ,set))

;;; foreign libraries

(cffi:define-foreign-library gecode-kernel
  (t (:default "libgecodekernel")))

(cffi:define-foreign-library gecode-support
  (t (:default "libgecodesupport")))

(cffi:define-foreign-library gecode-int
  (t (:default "libgecodeint")))

(cffi:define-foreign-library libgmp
  (t (:default "libgmp")))
(cffi:define-foreign-library libmpfr
  (t (:default "libmpfr")))

(cffi:define-foreign-library gecode-float
  (t (:default "libgecodefloat")))

(cffi:define-foreign-library gecode-set
  (t (:default "libgecodeset")))

(cffi:define-foreign-library gecode-search
  (t (:default "libgecodesearch")))

(cffi:define-foreign-library gecode-minimodel
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

(defvar *gluelib-pathname*
  (merge-pathnames #P"lib/" gecode-config:*base-directory*))

(pushnew *gluelib-pathname* cffi:*foreign-library-directories* :test #'equal)

(cffi:define-foreign-library gecode-glue
  (t (:default "libgecodeglue")))
(cffi:use-foreign-library gecode-glue)


