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


(cffi:define-foreign-library gecode-glue
  (t (:default "./lib/libgecodeglue")))
(cffi:use-foreign-library gecode-glue)


