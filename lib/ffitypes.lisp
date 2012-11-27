;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;; size_t

(defctype size :unsigned-int)

;;; modification events

(defcenum modevent-int
  (:var-failed -1)
  (:var-none 0)
  (:var-assigned 1)
  (:var-bounds 2)
  (:var-domain 3))


;;; store and variable types

(defcenum space-status
  (:ss-failed -2)
  (:ss-solved -1)
  (:ss-branch 0))

(defcenum variable-status
  (:space-failed -2)
  (:var-unassigned -1)
  (:var-assigned 0))

(defcenum reify-mode
  (:<=> 0) ; b=1 <=> C
  (:<== 1) ; b=1 <== C
  (:==> 2)); b=1 ==> C


;;; brancher types
#+(or)
(defcenum bvar-sel
  :bvar-none
  :bvar-rnd
  :bvar-degree-min
  :bvar-degree-max
  :bvar-afc-min
  :bvar-afc-max
  :bvar-min-min
  :bvar-min-max
  :bvar-max-min
  :bvar-max-max
  :bvar-size-min
  :bvar-size-max
  :bvar-size-degree-min
  :bvar-size-degree-max
  :bvar-size-afc-min
  :bvar-size-afc-max
  :bvar-regret-min-min
  :bvar-regret-min-max
  :bvar-regret-max-min
  :bvar-regret-max-max
  )

#+(or)
(defcenum bval-sel
  :bval-min
  :bval-med
  :bval-max
  :bval-rnd
  :bval-split-min
  :bval-split-max
  :bval-range-min
  :bval-range-max
  :bval-values-min
  :bval-values-max
  )


;;; propagator and operation types

(defcenum int-relation-type
  :irt-=
  :irt-/=
  :irt-<=
  :irt-<
  :irt->=
  :irt->)

(defcenum bool-operation-type
  :bot-and
  :bot-or
  :bot-imp
  :bot-eqv
  :bot-xor)

(defcenum int-consistency-level
  :icl-val                ; Value consistency (naive)
  :icl-bnd                ; Bounds consistency.
  :icl-dom                ; Domain consistency.
  :icl-def                ; The default consistency for a constraint.
  )

;;(defcenum set-rel-type
;;  :srt-eq                               ; equality
;;  :srt-nq                               ; disequality
;;  :srt-sub                              ; subset
;;  :srt-sup                              ; superset
;;  :srt-disj                             ; disjoint
;;  :srt-cmpl                             ; complement
;;  )


;;(defcenum set-op-type
;;  :sot-union                            ; union
;;  :sot-dunion                           ; disjoint union
;;  :sot-inter                            ; intersection
;;  :sot-minus                            ; difference
;;  )
