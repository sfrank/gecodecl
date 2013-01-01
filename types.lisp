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

(cffi:define-foreign-type space-type () ()
  (:actual-type :pointer)
  (:simple-parser space-type))

(defmethod expand-to-foreign (space (type space-type))
  `(gspace-sap ,space))

(cffi:define-foreign-type search-type () ()
  (:actual-type :pointer)
  (:simple-parser search-type))

(defmethod expand-to-foreign (engine (type search-type))
  `(engine-sap ,engine))

(cffi:define-foreign-type intvar-type () ()
  (:actual-type :pointer)
  (:simple-parser intvar-type))

(defmethod expand-to-foreign (intvar (type intvar-type))
  `(gecode_get_intvar_by_index *gspace* (intvar-index ,intvar)))

(cffi:define-foreign-type intvarargs-type () ()
  (:actual-type :pointer)
  (:simple-parser intvarargs-type))

;; ,var is the argument array
(defmethod expand-to-foreign-dyn (value var body (type intvarargs-type))
  `(let* ((length (length ,value))
          (,var (gecode_varargs_create length))
          (i -1))
     (declare (type fixnum i length))
     (map nil
          (lambda (x)
            (declare (type intvar x))
            (gecode_varargs_set ,var (incf i) x))
          ,value)
     (unwind-protect 
          (progn ,@body)
       (gecode_varargs_delete ,var))))

(cffi:define-foreign-type intargs-type () ()
  (:actual-type :pointer)
  (:simple-parser intargs-type))

;; ,var is the argument array
(defmethod expand-to-foreign-dyn (value var body (type intargs-type))
  `(let* ((length (length ,value))
          (,var (gecode_intargs_create length))
          (adr (gecode_intargs_adr ,var))
          (i -1))
     (declare (type fixnum i length))
     (map nil
          (lambda (x)
            (declare (type fixnum x))
            ;(gecode_intargs_set ,var (incf i) x)
            (setf (mem-aref adr :int (incf i)) x))
          ,value)
     (unwind-protect 
          (progn ,@body)
       (gecode_intargs_delete ,var))))


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


(pushnew #P"./lib/" *foreign-library-directories* :test #'equal)

(cffi:define-foreign-library gecode-glue
  (t (:default "libgecodeglue")))
(cffi:use-foreign-library gecode-glue)


