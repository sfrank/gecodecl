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

(defun reclaim-space (sap)
  (lambda ()
    ;;(format t "Space GCed...~%")
    (gecode_space_delete sap)))

(defun make-gspace ()
  (let ((space (%make-space)))
    (tg:finalize space (reclaim-space space))
    space))

(defun make-gspace-from-ref (sap)
  (let ((space (%make-space-boa sap nil)))
    (tg:finalize space (reclaim-space sap))
    space))

(defun copy-gspace (space)
  (declare (type gspace space))
  (let ((copy (%make-space-boa (gecode_space_copy space)
                               (gspace-int-notifiers space))))
    (tg:finalize space (reclaim-space copy))
    copy))

(defparameter *gspace* nil)


;;; CFFI type translators

(define-foreign-type space-type () ()
  (:actual-type :pointer)
  (:simple-parser space-type))

(defmethod expand-to-foreign (space (type space-type))
  `(gspace-sap ,space))

