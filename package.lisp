;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :cl-user)

(defpackage :gecodecl
  (:documentation "Bindings to GECODE.")
  (:use :cl :cffi)
  (:export
   #:%make-space
   #:make-space
   ))
