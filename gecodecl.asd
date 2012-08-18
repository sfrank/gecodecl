;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defpackage :gecodecl-system (:use :asdf :cl))
(in-package :gecodecl-system)

(defsystem :gecodecl
  :name "gecodecl"
  :description "gecodecl"
  :serial t
  :components
  ((:module package
    :pathname ""
    :components ((:file "package")))
   (:module types
    :depends-on (package)
    :pathname ""
    :components ((:file "types")))
   (:module gecodeglue
    :depends-on (package types)
    :pathname "lib"
    :components ((:file "ffitypes")
                 (:file "gecodeglue" :depends-on ("ffitypes"))))
   (:module base
    :depends-on (package types gecodeglue)
    :pathname ""
    :components ((:file "core")
                 ;(:file "simplifier" :depends-on ("core"))
                 (:file "interface" :depends-on ("core")))))
  :depends-on
  (:alexandria :cffi :trivial-garbage))
