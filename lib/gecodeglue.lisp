;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.7
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(in-package :gecodecl)



(cl:defconstant _GECODEGLUE_H 1)

(cffi:defcfun ("gecode_varargs_create" gecode_varargs_create) :pointer
  (n :int))

(cffi:defcfun ("gecode_varargs_set" gecode_varargs_set) :void
  (v :pointer)
  (i :int)
  (e intvar-type))

(cffi:defcfun ("gecode_varargs_delete" gecode_varargs_delete) :void
  (v :pointer))

(cffi:defcfun ("gecode_intargs_create" gecode_intargs_create) :pointer
  (n :int))

(cffi:defcfun ("gecode_intargs_adr" gecode_intargs_adr) :pointer
  (v :pointer))

(cffi:defcfun ("gecode_intargs_delete" gecode_intargs_delete) :void
  (v :pointer))

(cffi:defcfun ("gecode_floatargs_create" gecode_floatargs_create) :pointer
  (n :int))

(cffi:defcfun ("gecode_floatargs_set" gecode_floatargs_set) :void
  (v :pointer)
  (i :int)
  (e :double))

(cffi:defcfun ("gecode_floatargs_delete" gecode_floatargs_delete) :void
  (v :pointer))

(cffi:defcfun ("gecode_init_exceptionHandler" gecode_init_exceptionHandler) :void
  (fptr :pointer))

(cffi:defcfun ("gecode_init_callbackInt" gecode_init_callbackInt) :void
  (fptr :pointer))

(cffi:defcfun ("gecode_intClChannel" gecode_intClChannel) :void
  (space space-type)
  (x0 size)
  (idx :unsigned-int))

(cffi:defcfun ("gecode_space_create" gecode_space_create) :pointer)

(cffi:defcfun ("gecode_space_delete" gecode_space_delete) :void
  (space space-type))

(cffi:defcfun ("gecode_space_copy" gecode_space_copy) :pointer
  (space space-type))

(cffi:defcfun ("gecode_bool_addvar" gecode_bool_addvar) size
  (space space-type))

(cffi:defcfun ("gecode_int_addvar" gecode_int_addvar) size
  (space space-type)
  (min :int)
  (max :int))

(cffi:defcfun ("gecode_int_addvar_set" gecode_int_addvar_set) size
  (space space-type)
  (set intset-type))

(cffi:defcfun ("gecode_float_addvar" gecode_float_addvar) size
  (space space-type)
  (min :double)
  (max :double))

(cffi:defcfun ("gecode_get_boolvar_by_index" gecode_get_boolvar_by_index) :pointer
  (space space-type)
  (index size))

(cffi:defcfun ("gecode_get_intvar_by_index" gecode_get_intvar_by_index) :pointer
  (space space-type)
  (index size))

(cffi:defcfun ("gecode_get_floatvar_by_index" gecode_get_floatvar_by_index) :pointer
  (space space-type)
  (index size))

(cffi:defcfun ("gecode_get_bool_info" gecode_get_bool_info) variable-status
  (space space-type)
  (var boolvar-type)
  (value :pointer))

(cffi:defcfun ("gecode_get_int_info" gecode_get_int_info) variable-status
  (space space-type)
  (var intvar-type)
  (min :pointer)
  (max :pointer)
  (size :pointer))

(cffi:defcfun ("gecode_get_float_info" gecode_get_float_info) variable-status
  (space space-type)
  (var floatvar-type)
  (min :pointer)
  (max :pointer)
  (median :pointer))

(cffi:defcfun ("gecode_branch_ivar" gecode_branch_ivar) :void
  (space space-type)
  (var intvar-type)
  (valb intvalselector-type))

(cffi:defcfun ("gecode_branch_ivars" gecode_branch_ivars) :void
  (space space-type)
  (vars intvarargs-type)
  (varb intvarselector-type)
  (valb intvalselector-type))

(cffi:defcfun ("gecode_branch_bvar" gecode_branch_bvar) :void
  (space space-type)
  (var boolvar-type)
  (valb intvalselector-type))

(cffi:defcfun ("gecode_branch_bvars" gecode_branch_bvars) :void
  (space space-type)
  (vars boolvarargs-type)
  (varb intvarselector-type)
  (valb intvalselector-type))

(cffi:defcfun ("gecode_ivar_selector_delete" gecode_ivar_selector_delete) :void
  (s intvarselector-type))

(cffi:defcfun ("INT_VAR_NONE" INT_VAR_NONE) intvarselector-type)

(cffi:defcfun ("INT_VAR_RND" INT_VAR_RND) intvarselector-type
  (seed :unsigned-int))

(cffi:defcfun ("INT_VAR_DEGREE_MIN" INT_VAR_DEGREE_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_DEGREE_MAX" INT_VAR_DEGREE_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_AFC_MIN" INT_VAR_AFC_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_AFC_MAX" INT_VAR_AFC_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_MIN_MIN" INT_VAR_MIN_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_MIN_MAX" INT_VAR_MIN_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_MAX_MIN" INT_VAR_MAX_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_MAX_MAX" INT_VAR_MAX_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_SIZE_MIN" INT_VAR_SIZE_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_SIZE_MAX" INT_VAR_SIZE_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_SIZE_DEGREE_MIN" INT_VAR_SIZE_DEGREE_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_SIZE_DEGREE_MAX" INT_VAR_SIZE_DEGREE_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_SIZE_AFC_MIN" INT_VAR_SIZE_AFC_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_SIZE_AFC_MAX" INT_VAR_SIZE_AFC_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_REGRET_MIN_MIN" INT_VAR_REGRET_MIN_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_REGRET_MIN_MAX" INT_VAR_REGRET_MIN_MAX) intvarselector-type)

(cffi:defcfun ("INT_VAR_REGRET_MAX_MIN" INT_VAR_REGRET_MAX_MIN) intvarselector-type)

(cffi:defcfun ("INT_VAR_REGRET_MAX_MAX" INT_VAR_REGRET_MAX_MAX) intvarselector-type)

(cffi:defcfun ("gecode_ival_selector_delete" gecode_ival_selector_delete) :void
  (s intvalselector-type))

(cffi:defcfun ("INT_VAL_MIN" INT_VAL_MIN) intvalselector-type)

(cffi:defcfun ("INT_VAL_MED" INT_VAL_MED) intvalselector-type)

(cffi:defcfun ("INT_VAL_MAX" INT_VAL_MAX) intvalselector-type)

(cffi:defcfun ("INT_VAL_RND" INT_VAL_RND) intvalselector-type
  (seed :unsigned-int))

(cffi:defcfun ("INT_VAL_SPLIT_MIN" INT_VAL_SPLIT_MIN) intvalselector-type)

(cffi:defcfun ("INT_VAL_SPLIT_MAX" INT_VAL_SPLIT_MAX) intvalselector-type)

(cffi:defcfun ("INT_VAL_RANGE_MIN" INT_VAL_RANGE_MIN) intvalselector-type)

(cffi:defcfun ("INT_VAL_RANGE_MAX" INT_VAL_RANGE_MAX) intvalselector-type)

(cffi:defcfun ("INT_VALUES_MIN" INT_VALUES_MIN) intvalselector-type)

(cffi:defcfun ("INT_VALUES_MAX" INT_VALUES_MAX) intvalselector-type)

(cffi:defcfun ("gecode_dfs_engine_create" gecode_dfs_engine_create) :pointer
  (space space-type))

(cffi:defcfun ("gecode_dfs_engine_delete" gecode_dfs_engine_delete) :void
  (dfs search-type))

(cffi:defcfun ("gecode_dfs_engine_next" gecode_dfs_engine_next) :pointer
  (dfs search-type))

(cffi:defcfun ("gecode_bab_engine_create" gecode_bab_engine_create) :pointer
  (space space-type)
  (minVar size))

(cffi:defcfun ("gecode_bab_engine_delete" gecode_bab_engine_delete) :void
  (bab search-type))

(cffi:defcfun ("gecode_bab_engine_next" gecode_bab_engine_next) :pointer
  (bab search-type))

(cffi:defcfun ("gecode_rel_bvar_int" gecode_rel_bvar_int) :void
  (space space-type)
  (op int-relation-type)
  (v boolvar-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_bvar_bvar" gecode_rel_bvar_bvar) :void
  (space space-type)
  (op int-relation-type)
  (v1 boolvar-type)
  (v2 boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_bvar_int_reified" gecode_rel_bvar_int_reified) :void
  (space space-type)
  (op int-relation-type)
  (v1 boolvar-type)
  (val :int)
  (mode reify-mode)
  (v2 boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_bvar_bvar_reified" gecode_rel_bvar_bvar_reified) :void
  (space space-type)
  (op int-relation-type)
  (v1 boolvar-type)
  (v2 boolvar-type)
  (mode reify-mode)
  (b1 boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_bvars" gecode_rel_bvars) :void
  (space space-type)
  (op int-relation-type)
  (v boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_bvars_int" gecode_rel_bvars_int) :void
  (space space-type)
  (op int-relation-type)
  (v boolvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_bvars_bvars" gecode_rel_bvars_bvars) :void
  (space space-type)
  (op int-relation-type)
  (v1 boolvarargs-type)
  (v2 boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_op_bvar_bvar_bvar" gecode_op_bvar_bvar_bvar) :void
  (space space-type)
  (op bool-operation-type)
  (v1 boolvar-type)
  (v2 boolvar-type)
  (b1 boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_op_bvar_bvar_int" gecode_op_bvar_bvar_int) :void
  (space space-type)
  (op bool-operation-type)
  (v1 boolvar-type)
  (v2 boolvar-type)
  (n :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_op_bvars_int" gecode_op_bvars_int) :void
  (space space-type)
  (op bool-operation-type)
  (v boolvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_op_bvars_bvar" gecode_op_bvars_bvar) :void
  (space space-type)
  (op bool-operation-type)
  (v boolvarargs-type)
  (var boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_clause_bvars_bvars_int" gecode_clause_bvars_bvars_int) :void
  (space space-type)
  (op bool-operation-type)
  (v1 boolvarargs-type)
  (v2 boolvarargs-type)
  (n :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_clause_bvars_bvars_bvar" gecode_clause_bvars_bvars_bvar) :void
  (space space-type)
  (op bool-operation-type)
  (v1 boolvarargs-type)
  (v2 boolvarargs-type)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivar_int" gecode_dom_ivar_int) :void
  (space space-type)
  (v intvar-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivars_int" gecode_dom_ivars_int) :void
  (space space-type)
  (v intvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivar_int_int" gecode_dom_ivar_int_int) :void
  (space space-type)
  (v intvar-type)
  (l :int)
  (m :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivars_int_int" gecode_dom_ivars_int_int) :void
  (space space-type)
  (v intvarargs-type)
  (l :int)
  (m :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivar_iset" gecode_dom_ivar_iset) :void
  (space space-type)
  (v intvar-type)
  (s intset-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivars_iset" gecode_dom_ivars_iset) :void
  (space space-type)
  (v intvarargs-type)
  (s intset-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivar_int_reify" gecode_dom_ivar_int_reify) :void
  (space space-type)
  (v intvar-type)
  (value :int)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivar_int_int_reify" gecode_dom_ivar_int_int_reify) :void
  (space space-type)
  (v intvar-type)
  (l :int)
  (m :int)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_dom_ivar_iset_reify" gecode_dom_ivar_iset_reify) :void
  (space space-type)
  (v intvar-type)
  (s intset-type)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivar_int" gecode_rel_ivar_int) :void
  (space space-type)
  (op int-relation-type)
  (v intvar-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivar_ivar" gecode_rel_ivar_ivar) :void
  (space space-type)
  (op int-relation-type)
  (v1 intvar-type)
  (v2 intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivar_int_reified" gecode_rel_ivar_int_reified) :void
  (space space-type)
  (op int-relation-type)
  (v1 intvar-type)
  (val :int)
  (mode reify-mode)
  (b1 boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivar_ivar_reified" gecode_rel_ivar_ivar_reified) :void
  (space space-type)
  (op int-relation-type)
  (v1 intvar-type)
  (v2 intvar-type)
  (mode reify-mode)
  (b1 boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivars" gecode_rel_ivars) :void
  (space space-type)
  (op int-relation-type)
  (v intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivars_int" gecode_rel_ivars_int) :void
  (space space-type)
  (op int-relation-type)
  (v intvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_ivars_ivars" gecode_rel_ivars_ivars) :void
  (space space-type)
  (op int-relation-type)
  (v1 intvarargs-type)
  (v2 intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_distinct_ivars" gecode_distinct_ivars) :void
  (space space-type)
  (va intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_distinct_ints_ivars" gecode_distinct_ints_ivars) :void
  (space space-type)
  (ia intargs-type)
  (va intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_sorted_ivars_ivars" gecode_sorted_ivars_ivars) :void
  (space space-type)
  (xvars intvarargs-type)
  (yvars intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_sorted_ivars_ivars_ivars" gecode_sorted_ivars_ivars_ivars) :void
  (space space-type)
  (xvars intvarargs-type)
  (yvars intvarargs-type)
  (zvars intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_binpacking" gecode_binpacking) :void
  (space space-type)
  (l intvarargs-type)
  (b intvarargs-type)
  (s intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_nooverlap" gecode_nooverlap) :void
  (space space-type)
  (x intvarargs-type)
  (w intargs-type)
  (y intvarargs-type)
  (h intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_nooverlap_optional" gecode_nooverlap_optional) :void
  (space space-type)
  (x intvarargs-type)
  (w intargs-type)
  (y intvarargs-type)
  (h intargs-type)
  (o boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_nooverlap_coords" gecode_nooverlap_coords) :void
  (space space-type)
  (x0 intvarargs-type)
  (w intvarargs-type)
  (x1 intvarargs-type)
  (y0 intvarargs-type)
  (h intvarargs-type)
  (y1 intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_nooverlap_coords_optional" gecode_nooverlap_coords_optional) :void
  (space space-type)
  (x0 intvarargs-type)
  (w intvarargs-type)
  (x1 intvarargs-type)
  (y0 intvarargs-type)
  (h intvarargs-type)
  (y1 intvarargs-type)
  (o boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulatives_ivars" gecode_cumulatives_ivars) :void
  (space space-type)
  (m intvarargs-type)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (u intvarargs-type)
  (c intargs-type)
  (at_most :boolean)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_unary_ivars" gecode_unary_ivars) :void
  (space space-type)
  (s intvarargs-type)
  (p intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_unary_ivars_bvars" gecode_unary_ivars_bvars) :void
  (space space-type)
  (s intvarargs-type)
  (p intargs-type)
  (b boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_unary_tasks_ivars" gecode_unary_tasks_ivars) :void
  (space space-type)
  (tasks tasktypeargs-type)
  (flex intvarargs-type)
  (fix intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_unary_tasks_ivars_bvars" gecode_unary_tasks_ivars_bvars) :void
  (space space-type)
  (tasks tasktypeargs-type)
  (flex intvarargs-type)
  (fix intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_unary_ivars_ivars" gecode_unary_ivars_ivars) :void
  (space space-type)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_unary_ivars_ivars_bvars" gecode_unary_ivars_ivars_bvars) :void
  (space space-type)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_int_tasks" gecode_cumulative_int_tasks) :void
  (space space-type)
  (c :int)
  (t_arg2 tasktypeargs-type)
  (flex intvarargs-type)
  (fix intargs-type)
  (u intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_ivar_tasks" gecode_cumulative_ivar_tasks) :void
  (space space-type)
  (c intvar-type)
  (t_arg2 tasktypeargs-type)
  (flex intvarargs-type)
  (fix intargs-type)
  (u intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_int_tasks_bvars" gecode_cumulative_int_tasks_bvars) :void
  (space space-type)
  (c :int)
  (t_arg2 tasktypeargs-type)
  (flex intvarargs-type)
  (fix intargs-type)
  (u intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_ivar_tasks_bvars" gecode_cumulative_ivar_tasks_bvars) :void
  (space space-type)
  (c intvar-type)
  (t_arg2 tasktypeargs-type)
  (flex intvarargs-type)
  (fix intargs-type)
  (u intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_int" gecode_cumulative_int) :void
  (space space-type)
  (c :int)
  (s intvarargs-type)
  (p intargs-type)
  (u intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_ivar" gecode_cumulative_ivar) :void
  (space space-type)
  (c intvar-type)
  (s intvarargs-type)
  (p intargs-type)
  (u intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_int_bvars" gecode_cumulative_int_bvars) :void
  (space space-type)
  (c :int)
  (s intvarargs-type)
  (p intargs-type)
  (u intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_ivar_bvars" gecode_cumulative_ivar_bvars) :void
  (space space-type)
  (c intvar-type)
  (s intvarargs-type)
  (p intargs-type)
  (u intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_int_ivars" gecode_cumulative_int_ivars) :void
  (space space-type)
  (c :int)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (u intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_ivar_ivars" gecode_cumulative_ivar_ivars) :void
  (space space-type)
  (c intvar-type)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (u intargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_int_ivars_bvars" gecode_cumulative_int_ivars_bvars) :void
  (space space-type)
  (c :int)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (u intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_cumulative_ivar_ivars_bvars" gecode_cumulative_ivar_ivars_bvars) :void
  (space space-type)
  (c intvar-type)
  (s intvarargs-type)
  (p intvarargs-type)
  (e intvarargs-type)
  (u intargs-type)
  (m boolvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_circuit_ivars_costs" gecode_circuit_ivars_costs) :void
  (space space-type)
  (c intargs-type)
  (x intvarargs-type)
  (y intvarargs-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_circuit_ivars_costs_offset" gecode_circuit_ivars_costs_offset) :void
  (space space-type)
  (c intargs-type)
  (offset :int)
  (x intvarargs-type)
  (y intvarargs-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_circuit_ivars" gecode_circuit_ivars) :void
  (space space-type)
  (c intargs-type)
  (x intvarargs-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_circuit_ivars_offset" gecode_circuit_ivars_offset) :void
  (space space-type)
  (c intargs-type)
  (offset :int)
  (x intvarargs-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_path_ivars" gecode_path_ivars) :void
  (space space-type)
  (x intvarargs-type)
  (s intvar-type)
  (e intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_path_ivars_offset" gecode_path_ivars_offset) :void
  (space space-type)
  (offset :int)
  (x intvarargs-type)
  (s intvar-type)
  (e intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_path_ivars_edge_costs" gecode_path_ivars_edge_costs) :void
  (space space-type)
  (c intargs-type)
  (x intvarargs-type)
  (s intvar-type)
  (e intvar-type)
  (y intvarargs-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_path_ivars_edge_costs_offset" gecode_path_ivars_edge_costs_offset) :void
  (space space-type)
  (c intargs-type)
  (offset :int)
  (x intvarargs-type)
  (s intvar-type)
  (e intvar-type)
  (y intvarargs-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_path_ivars_costs" gecode_path_ivars_costs) :void
  (space space-type)
  (c intargs-type)
  (x intvarargs-type)
  (s intvar-type)
  (e intvar-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_path_ivars_costs_offset" gecode_path_ivars_costs_offset) :void
  (space space-type)
  (c intargs-type)
  (offset :int)
  (x intvarargs-type)
  (s intvar-type)
  (e intvar-type)
  (z intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_channel_ivars_ivars" gecode_channel_ivars_ivars) :void
  (space space-type)
  (v1 intvarargs-type)
  (v2 intvarargs-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_channel_ivars_int_ivars_int" gecode_channel_ivars_int_ivars_int) :void
  (space space-type)
  (v1 intvarargs-type)
  (xoff :int)
  (v2 intvarargs-type)
  (yoff :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_channel_bvar_ivar" gecode_channel_bvar_ivar) :void
  (space space-type)
  (bvar boolvar-type)
  (ivar intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_channel_ivar_bvar" gecode_channel_ivar_bvar) :void
  (space space-type)
  (ivar intvar-type)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_channel_bvars_ivar_int" gecode_channel_bvars_ivar_int) :void
  (space space-type)
  (v1 boolvarargs-type)
  (ivar intvar-type)
  (offset :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_min_ivar_ivar_ivar" gecode_min_ivar_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (ret intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_min_ivars_ivar" gecode_min_ivars_ivar) :void
  (space space-type)
  (v intvarargs-type)
  (y intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_max_ivar_ivar_ivar" gecode_max_ivar_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (ret intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_max_ivars_ivar" gecode_max_ivars_ivar) :void
  (space space-type)
  (v intvarargs-type)
  (y intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_abs_ivar_ivar" gecode_abs_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_mul_ivar_ivar_ivar" gecode_mul_ivar_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (ret intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_sqr_ivar_ivar" gecode_sqr_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_sqrt_ivar_ivar" gecode_sqrt_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_divmod_ivar_ivar_ivar_ivar" gecode_divmod_ivar_ivar_ivar_ivar) :void
  (space space-type)
  (x0 intvar-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (x3 intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_div_ivar_ivar_ivar" gecode_div_ivar_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (ret intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_mod_ivar_ivar_ivar" gecode_mod_ivar_ivar_ivar) :void
  (space space-type)
  (x1 intvar-type)
  (x2 intvar-type)
  (ret intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ivars_int" gecode_lin_ivars_int) :void
  (space space-type)
  (rel int-relation-type)
  (v intvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ivars_ivar" gecode_lin_ivars_ivar) :void
  (space space-type)
  (rel int-relation-type)
  (v intvarargs-type)
  (var intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ivars_int_reified" gecode_lin_ivars_int_reified) :void
  (space space-type)
  (rel int-relation-type)
  (v intvarargs-type)
  (value :int)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ivars_ivar_reified" gecode_lin_ivars_ivar_reified) :void
  (space space-type)
  (rel int-relation-type)
  (v intvarargs-type)
  (var intvar-type)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_ivars_int" gecode_lin_ints_ivars_int) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v intvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_ivars_ivar" gecode_lin_ints_ivars_ivar) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v intvarargs-type)
  (var intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_ivars_int_reified" gecode_lin_ints_ivars_int_reified) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v intvarargs-type)
  (value :int)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_ivars_ivar_reified" gecode_lin_ints_ivars_ivar_reified) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v intvarargs-type)
  (var intvar-type)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_bvars_int" gecode_lin_bvars_int) :void
  (space space-type)
  (rel int-relation-type)
  (v boolvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_bvars_ivar" gecode_lin_bvars_ivar) :void
  (space space-type)
  (rel int-relation-type)
  (v boolvarargs-type)
  (var intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_bvars_int_reified" gecode_lin_bvars_int_reified) :void
  (space space-type)
  (rel int-relation-type)
  (v boolvarargs-type)
  (value :int)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_bvars_ivar_reified" gecode_lin_bvars_ivar_reified) :void
  (space space-type)
  (rel int-relation-type)
  (v boolvarargs-type)
  (var intvar-type)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_bvars_int" gecode_lin_ints_bvars_int) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v boolvarargs-type)
  (value :int)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_bvars_ivar" gecode_lin_ints_bvars_ivar) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v boolvarargs-type)
  (var intvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_bvars_int_reified" gecode_lin_ints_bvars_int_reified) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v boolvarargs-type)
  (value :int)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_lin_ints_bvars_ivar_reified" gecode_lin_ints_bvars_ivar_reified) :void
  (space space-type)
  (rel int-relation-type)
  (ints intargs-type)
  (v boolvarargs-type)
  (var intvar-type)
  (mode reify-mode)
  (bvar boolvar-type)
  (icl int-consistency-level))

(cffi:defcfun ("gecode_rel_fvar_fvar" gecode_rel_fvar_fvar) :void
  (space space-type)
  (op float-relation-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_rel_fvar_dbl" gecode_rel_fvar_dbl) :void
  (space space-type)
  (op float-relation-type)
  (x0 floatvar-type)
  (x1 :double))

(cffi:defcfun ("gecode_rel_fvar_dbl_reified" gecode_rel_fvar_dbl_reified) :void
  (space space-type)
  (op float-relation-type)
  (x0 floatvar-type)
  (x1 :double)
  (mode reify-mode)
  (bvar boolvar-type))

(cffi:defcfun ("gecode_rel_fvar_fvar_reified" gecode_rel_fvar_fvar_reified) :void
  (space space-type)
  (op float-relation-type)
  (x0 floatvar-type)
  (x1 floatvar-type)
  (mode reify-mode)
  (bvar boolvar-type))

(cffi:defcfun ("gecode_rel_fvars_dbl" gecode_rel_fvars_dbl) :void
  (space space-type)
  (op float-relation-type)
  (x0 floatvarargs-type)
  (x1 :double))

(cffi:defcfun ("gecode_rel_fvars_fvar" gecode_rel_fvars_fvar) :void
  (space space-type)
  (op float-relation-type)
  (x0 floatvarargs-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_min_fvar_fvar_fvar" gecode_min_fvar_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type)
  (x2 floatvar-type))

(cffi:defcfun ("gecode_min_fvars_fvar" gecode_min_fvars_fvar) :void
  (space space-type)
  (x floatvarargs-type)
  (y floatvar-type))

(cffi:defcfun ("gecode_max_fvar_fvar_fvar" gecode_max_fvar_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type)
  (x2 floatvar-type))

(cffi:defcfun ("gecode_max_fvars_fvar" gecode_max_fvars_fvar) :void
  (space space-type)
  (x floatvarargs-type)
  (y floatvar-type))

(cffi:defcfun ("gecode_abs_fvar_fvar" gecode_abs_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_mult_fvar_fvar_fvar" gecode_mult_fvar_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type)
  (x2 floatvar-type))

(cffi:defcfun ("gecode_sqr_fvar_fvar" gecode_sqr_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_sqrt_fvar_fvar" gecode_sqrt_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_pow_fvar_uint_fvar" gecode_pow_fvar_uint_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (pow :unsigned-int)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_nroot_fvar_uint_fvar" gecode_nroot_fvar_uint_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (pow :unsigned-int)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_div_fvar_fvar_fvar" gecode_div_fvar_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type)
  (x2 floatvar-type))

(cffi:defcfun ("gecode_exp_fvar_fvar" gecode_exp_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_log_fvar_fvar" gecode_log_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_exp_dbl_fvar_fvar" gecode_exp_dbl_fvar_fvar) :void
  (space space-type)
  (base :double)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_log_dbl_fvar_fvar" gecode_log_dbl_fvar_fvar) :void
  (space space-type)
  (base :double)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_asin_fvar_fvar" gecode_asin_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_sin_fvar_fvar" gecode_sin_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_acos_fvar_fvar" gecode_acos_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_cos_fvar_fvar" gecode_cos_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_atan_fvar_fvar" gecode_atan_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_tan_fvar_fvar" gecode_tan_fvar_fvar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 floatvar-type))

(cffi:defcfun ("gecode_lin_fvars_dbl" gecode_lin_fvars_dbl) :void
  (space space-type)
  (rel float-relation-type)
  (x floatvarargs-type)
  (c :double))

(cffi:defcfun ("gecode_lin_fvars_fvar" gecode_lin_fvars_fvar) :void
  (space space-type)
  (rel float-relation-type)
  (x floatvarargs-type)
  (y floatvar-type))

(cffi:defcfun ("gecode_lin_fvars_dbl_reified" gecode_lin_fvars_dbl_reified) :void
  (space space-type)
  (rel float-relation-type)
  (x floatvarargs-type)
  (c :double)
  (mode reify-mode)
  (bvar boolvar-type))

(cffi:defcfun ("gecode_lin_fvars_fvar_reified" gecode_lin_fvars_fvar_reified) :void
  (space space-type)
  (rel float-relation-type)
  (x floatvarargs-type)
  (y floatvar-type)
  (mode reify-mode)
  (bvar boolvar-type))

(cffi:defcfun ("gecode_lin_fargs_fvars_dbl" gecode_lin_fargs_fvars_dbl) :void
  (space space-type)
  (rel float-relation-type)
  (a floatargs-type)
  (x floatvarargs-type)
  (c :double))

(cffi:defcfun ("gecode_lin_fargs_fvars_fvar" gecode_lin_fargs_fvars_fvar) :void
  (space space-type)
  (rel float-relation-type)
  (a floatargs-type)
  (x floatvarargs-type)
  (y floatvar-type))

(cffi:defcfun ("gecode_lin_fargs_fvars_dbl_reified" gecode_lin_fargs_fvars_dbl_reified) :void
  (space space-type)
  (rel float-relation-type)
  (a floatargs-type)
  (x floatvarargs-type)
  (c :double)
  (mode reify-mode)
  (bvar boolvar-type))

(cffi:defcfun ("gecode_lin_fargs_fvars_fvar_reified" gecode_lin_fargs_fvars_fvar_reified) :void
  (space space-type)
  (rel float-relation-type)
  (a floatargs-type)
  (x floatvarargs-type)
  (y floatvar-type)
  (mode reify-mode)
  (bvar boolvar-type))

(cffi:defcfun ("gecode_channel_fvar_ivar" gecode_channel_fvar_ivar) :void
  (space space-type)
  (x0 floatvar-type)
  (x1 intvar-type))

(cffi:defcfun ("gecode_branch_fvar" gecode_branch_fvar) :void
  (space space-type)
  (var floatvar-type)
  (valb floatvalselector-type))

(cffi:defcfun ("gecode_branch_fvars" gecode_branch_fvars) :void
  (space space-type)
  (vars floatvarargs-type)
  (varb floatvarselector-type)
  (valb floatvalselector-type))

(cffi:defcfun ("gecode_fvar_selector_delete" gecode_fvar_selector_delete) :void
  (s floatvarselector-type))

(cffi:defcfun ("FLOAT_VAR_NONE" FLOAT_VAR_NONE) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_RND" FLOAT_VAR_RND) floatvarselector-type
  (seed :unsigned-int))

(cffi:defcfun ("FLOAT_VAR_DEGREE_MIN" FLOAT_VAR_DEGREE_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_DEGREE_MAX" FLOAT_VAR_DEGREE_MAX) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_AFC_MIN" FLOAT_VAR_AFC_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_AFC_MAX" FLOAT_VAR_AFC_MAX) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_MIN_MIN" FLOAT_VAR_MIN_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_MIN_MAX" FLOAT_VAR_MIN_MAX) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_MAX_MIN" FLOAT_VAR_MAX_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_MAX_MAX" FLOAT_VAR_MAX_MAX) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_SIZE_MIN" FLOAT_VAR_SIZE_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_SIZE_MAX" FLOAT_VAR_SIZE_MAX) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_SIZE_DEGREE_MIN" FLOAT_VAR_SIZE_DEGREE_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_SIZE_DEGREE_MAX" FLOAT_VAR_SIZE_DEGREE_MAX) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_SIZE_AFC_MIN" FLOAT_VAR_SIZE_AFC_MIN) floatvarselector-type)

(cffi:defcfun ("FLOAT_VAR_SIZE_AFC_MAX" FLOAT_VAR_SIZE_AFC_MAX) floatvarselector-type)

(cffi:defcfun ("gecode_fval_selector_delete" gecode_fval_selector_delete) :void
  (s floatvalselector-type))

(cffi:defcfun ("FLOAT_VAL_SPLIT_MIN" FLOAT_VAL_SPLIT_MIN) floatvalselector-type)

(cffi:defcfun ("FLOAT_VAL_SPLIT_MAX" FLOAT_VAL_SPLIT_MAX) floatvalselector-type)

(cffi:defcfun ("FLOAT_VAL_SPLIT_RND" FLOAT_VAL_SPLIT_RND) floatvalselector-type
  (seed :unsigned-int))

(cffi:defcfun ("gecode_intset_bounds" gecode_intset_bounds) intset-type
  (min :int)
  (max :int))

(cffi:defcfun ("gecode_intset_seq" gecode_intset_seq) intset-type
  (seq :pointer)
  (count :int))

(cffi:defcfun ("gecode_intset_ranges" gecode_intset_ranges) intset-type
  (seq :pointer)
  (count :int))

(cffi:defcfun ("gecode_intset_delete" gecode_intset_delete) :void
  (iset intset-type))


