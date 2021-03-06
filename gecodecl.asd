;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defpackage #:gecode-config (:export #:*base-directory*))
(defparameter gecode-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defpackage :gecodecl-system (:use :asdf :cl))
(in-package :gecodecl-system)

(defsystem :gecodecl
  :name "gecodecl"
  :description "gecodecl"
  :serial t
  :components
  ((:module base
    :pathname ""
    :components ((:file "package")
                 (:file "types" :depends-on ("package"))
                 (:file "lib/ffitypes" :depends-on ("package"))
                 (:file "lib/gecodeglue" :depends-on ("types" "lib/ffitypes"))
                 (:file "core" :depends-on ("lib/gecodeglue"))
                 (:file "interface" :depends-on ("core"))
                 ;(:file "simplifier" :depends-on ("core" "interface"))
                 )))
  :depends-on
  (:alexandria :cffi :trivial-garbage))
