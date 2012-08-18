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