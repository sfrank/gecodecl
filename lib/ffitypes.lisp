;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;; size_t

(cffi:defctype size :unsigned-int)

;;; modification events

(cffi:defcenum modevent-int
  (:var-failed -1)
  (:var-none 0)
  (:var-assigned 1)
  (:var-bounds 2)
  (:var-domain 3))


;;; store and variable types

(cffi:defcenum space-status
  (:ss-failed -2)
  (:ss-solved -1)
  (:ss-branch 0))

(cffi:defcenum variable-status
  (:space-failed -2)
  (:var-unassigned -1)
  (:var-assigned 0))

(cffi:defcenum reify-mode
  (:<=> 0)   ; b=1 <=> C
  (:<== 1)   ; b=1 <== C
  (:==> 2))  ; b=1 ==> C


;;; propagator and operation types

(cffi:defcenum int-relation-type
  :irt-=
  :irt-/=
  :irt-<=
  :irt-<
  :irt->=
  :irt->)

(cffi:defcenum bool-operation-type
  :bot-and
  :bot-or
  :bot-imp
  :bot-eqv
  :bot-xor)

(cffi:defcenum int-consistency-level
  :icl-val                ; Value consistency (naive)
  :icl-bnd                ; Bounds consistency.
  :icl-dom                ; Domain consistency.
  :icl-def                ; The default consistency for a constraint.
  )

(cffi:defcenum task-type
  :tt-fixp
  :tt-fixs
  :tt-fixe)

;;; DFA/extensional constraint fundamentals

;; This corresponds to the Transition class in int.hh, we can use this
;; special cstruct/class equivalence because the following conditions
;; hold which make the class structure POD (plain old data):
;;   * All data members are public and themselves POD or fundamental
;;     types (but not reference or pointer-to-member types), or arrays
;;     of such
;;   * It has no user-defined constructors, assignment operators or
;;     destructors
;;   * It has no virtual functions
;;   * It has no base classes

(cffi:defcstruct transition
  "Specification of a DFA transition."
  (i_state :int)
  (symbol :int)
  (o_state :int))

(cffi:defcenum extensional-prop-kind
  :epk-def           ; make a default decision
  :epk-speed         ; prefer speed over mmeory consumption
  :epk-memory        ; prefer little memory over speed
  )


;; set variable relations

(cffi:defcenum set-rel-type
  :srt-=         ; equality
  :srt-/=        ; disequality
  :srt-sub       ; subset
  :srt-sup       ; superset
  :srt-disj      ; disjoint
  :srt-cmpl      ; complement
  :srt-<=        ; less or equal
  :srt-<         ; less
  :srt->=        ; greater or equal
  :srt->         ; greater  
  )

(cffi:defcenum set-op-type
  :sot-union               ; union
  :sot-dunion              ; disjoint union
  :sot-inter               ; intersection
  :sot-minus               ; difference
  )


;; float variable relations

(cffi:defcenum float-relation-type
  :frt-=
  :frt-/=
  :frt-<=
  :frt-<
  :frt->=
  :frt->
  )
