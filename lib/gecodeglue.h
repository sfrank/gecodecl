#ifndef _GECODEGLUE_H
#define _GECODEGLUE_H       1

extern "C" {

  /* slightly extended IntVarArgs vectors though by using the FFI we
     will use them for all variable types where it is possible; this
     is a kludge and circumvents the C++ type checker but makes our
     life a lot easier since we can now use CFFI type translators and
     don't have to hand code all the conversions by hand in C.
     (Though it is slightly slower since the argument array is not
     stack allocated) */
  CLVarArgs* gecode_varargs_create(int n);
  void gecode_varargs_set(CLVarArgs *v, int i, const IntVar* e);
  void gecode_varargs_delete(CLVarArgs *v);
  void gecode_varargs_delete_sap(void *v);

  /* integer argument arrays */
  CLIntArgs* gecode_intargs_create(int n);
  int *gecode_intargs_adr(CLIntArgs *v);
  void gecode_intargs_delete(CLIntArgs *v);
  void gecode_intargs_delete_sap(void *v);

  /* double argument arrays */
  CLFloatArgs* gecode_floatargs_create(int n);
  void gecode_floatargs_set(CLFloatArgs *v, int i, double e);
  void gecode_floatargs_delete(CLFloatArgs *v);
  void gecode_floatargs_delete_sap(void *v);

  /* IntSet argument arrays */
  CLIntSetArgs* gecode_intsetargs_create(int n);
  void gecode_intsetargs_set(CLIntSetArgs *v, int i, const IntSet* e);
  void gecode_intsetargs_delete(CLIntSetArgs *v);
  void gecode_intsetargs_delete_sap(void *v);

  /* SymmetryHandle argument array */
  CLSymmetryHandleArgs* gecode_symhandleargs_create(int n);
  void gecode_symhandleargs_set(CLSymmetryHandleArgs *v, int i, const SymmetryHandle* e);
  void gecode_symhandleargs_delete(CLSymmetryHandleArgs *v);
  void gecode_symhandleargs_delete_sap(void *v);


  /* exceptions */
  void gecode_init_exceptionHandler(void (*fptr)(const char*));

  /* callbacks */
  void gecode_init_callbackInt(void (*fptr)(unsigned, const ModEvent));

  /* space handling */
  void gecode_intClChannel(CLSpace* space, size_t x0, unsigned idx);
  CLSpace* gecode_space_create(void);
  void gecode_space_delete(CLSpace* space);
  void gecode_space_delete_sap(void* space);
  CLSpace* gecode_space_copy(CLSpace* space);
  unsigned int gecode_space_propagators_count(CLSpace* Space);
  unsigned int gecode_space_branchers_count(CLSpace* Space);

  /* variables */
  size_t gecode_bool_addvar(CLSpace *space);
  size_t gecode_int_addvar(CLSpace *space, int min, int max);
  size_t gecode_int_addvar_set(CLSpace *space, IntSet* set);
  size_t gecode_float_addvar(CLSpace *space, double min, double max);
  size_t gecode_set_addvar_plain(CLSpace *space,
                                 int glbMin,int glbMax,
                                 int lubMin,int lubMax,
                                 unsigned int cardMin,
                                 unsigned int cardMax);
  size_t gecode_set_addvar_sets(CLSpace *space,
                                IntSet* glbD, IntSet* lubD,
                                unsigned int cardMin,
                                unsigned int cardMax);
  BoolVar* gecode_get_boolvar_by_index(CLSpace *space, size_t index);
  IntVar* gecode_get_intvar_by_index(CLSpace *space, size_t index);
  FloatVar* gecode_get_floatvar_by_index(CLSpace *space, size_t index);
  SetVar* gecode_get_setvar_by_index(CLSpace *space, size_t index);
  STATUS gecode_get_bool_info(CLSpace *space, BoolVar* var, int *value);
  STATUS gecode_get_int_info(CLSpace *space, IntVar* var, 
			     int *min, int *max, int *size);
  STATUS gecode_get_float_info(CLSpace *space, FloatVar* var,
			       double *min, double *max, double *median);
  STATUS gecode_get_set_info(CLSpace *space,
                             SetVar* var,
                             int* lubMin, int* lubMax,
                             int* glbMin, int* glbMax,
                             int* cardMin, int* cardMax);

  /* symmetry handle */
  void gecode_symmetryhandle_delete(SymmetryHandle* sh);
  void gecode_symmetryhandle_delete_sap(void* sh);

  /* brancher handles */
  void gecode_brancherhandle_delete(BrancherHandle* bh);
  void gecode_brancherhandle_delete_sap(void* bh);
  void gecode_brancherhandle_kill(BrancherHandle* bh, CLSpace* space);


  /* branchers */
  BrancherHandle* gecode_branch_ivar(CLSpace *space, IntVar* var, IntValBranch* valb);
  BrancherHandle* gecode_branch_ivars(CLSpace *space, IntVarArgs* vars,
                                      IntVarBranch* varb, IntValBranch* valb);
  BrancherHandle* gecode_branch_ivars_tie(CLSpace *space, IntVarArgs* vars,
                                          IntVarBranch** varb, unsigned int cnt,
                                          IntValBranch* valb);
  BrancherHandle* gecode_branch_bvar(CLSpace *space, BoolVar* var, IntValBranch* valb);
  BrancherHandle* gecode_branch_bvars(CLSpace *space, BoolVarArgs* vars,
                                      IntVarBranch* varb, IntValBranch* valb);
  BrancherHandle* gecode_branch_bvars_tie(CLSpace *space, BoolVarArgs* vars,
                                          IntVarBranch** varb, unsigned int cnt,
                                          IntValBranch* valb);

  /* symmetries */
  SymmetryHandle* gecode_VariableSymmetry_ivars(IntVarArgs* x);
  SymmetryHandle* gecode_VariableSymmetry_bvars(BoolVarArgs* x);
  SymmetryHandle* gecode_VariableSymmetry_ivars_ints(IntVarArgs* x,
                                                     IntArgs* indices);
  SymmetryHandle* gecode_ValueSymmetry_ints(IntArgs* x);
  SymmetryHandle* gecode_ValueSymmetry_iset(IntSet* x);
  SymmetryHandle* gecode_ValueSymmetry_ivar(IntVar* var);
  SymmetryHandle* gecode_VariableSequenceSymmetry_ivars_int(IntVarArgs* x,
                                                            int ss);
  SymmetryHandle* gecode_VariableSequenceSymmetry_bvars_int(BoolVarArgs* x,
                                                            int ss);
  SymmetryHandle* gecode_ValueSequenceSymmetry_ints_int(IntArgs* x, int ss);
  SymmetryHandle* gecode_values_reflect_int_int(int lower, int upper);
  SymmetryHandle* gecode_values_reflect_ivar(IntVar* x);

  /* branchers with symmetries */
  BrancherHandle* gecode_branch_ivars_sym(CLSpace *space, IntVarArgs* vars,
                                          IntVarBranch* varb, IntValBranch* valb,
                                          Symmetries* sym);
  BrancherHandle* gecode_branch_bvars_sym(CLSpace *space, BoolVarArgs* vars,
                                          IntVarBranch* varb, IntValBranch* valb,
                                          Symmetries* sym);

  /* variable selectors for branchers */
  void gecode_ivar_selector_delete(IntVarBranch* s);
  void gecode_ivar_selector_delete_sap(void* s);
  IntVarBranch* INT_VAR_NONE(void);
  IntVarBranch* INT_VAR_RND(unsigned int seed);
  /* TODO: check whether to insert MERIT variants */
  IntVarBranch* INT_VAR_DEGREE_MIN(void);
  IntVarBranch* INT_VAR_DEGREE_MAX(void);
  IntVarBranch* INT_VAR_AFC_MIN(void);
  IntVarBranch* INT_VAR_AFC_MAX(void);
  /* TODO: check whether to insert ACTIVITY variants */
  IntVarBranch* INT_VAR_MIN_MIN(void);
  IntVarBranch* INT_VAR_MIN_MAX(void);
  IntVarBranch* INT_VAR_MAX_MIN(void);
  IntVarBranch* INT_VAR_MAX_MAX(void);
  IntVarBranch* INT_VAR_SIZE_MIN(void);
  IntVarBranch* INT_VAR_SIZE_MAX(void);
  IntVarBranch* INT_VAR_DEGREE_SIZE_MIN(void);
  IntVarBranch* INT_VAR_DEGREE_SIZE_MAX(void);
  IntVarBranch* INT_VAR_SIZE_AFC_MIN(void);
  IntVarBranch* INT_VAR_SIZE_AFC_MAX(void);
  /* TODO: check whether to insert ACTIVITY variants */
  IntVarBranch* INT_VAR_REGRET_MIN_MIN(void);
  IntVarBranch* INT_VAR_REGRET_MIN_MAX(void);
  IntVarBranch* INT_VAR_REGRET_MAX_MIN(void);
  IntVarBranch* INT_VAR_REGRET_MAX_MAX(void);

  /* value selectors for branchers */
  void gecode_ival_selector_delete(IntValBranch* s);
  void gecode_ival_selector_delete_sap(void* s);
  IntValBranch* INT_VAL_MIN(void);
  IntValBranch* INT_VAL_MED(void);
  IntValBranch* INT_VAL_MAX(void);
  IntValBranch* INT_VAL_RND(unsigned int seed);
  IntValBranch* INT_VAL_SPLIT_MIN(void);
  IntValBranch* INT_VAL_SPLIT_MAX(void);
  IntValBranch* INT_VAL_RANGE_MIN(void);
  IntValBranch* INT_VAL_RANGE_MAX(void);
  IntValBranch* INT_VALUES_MIN(void);
  IntValBranch* INT_VALUES_MAX(void);
  IntValBranch* INT_VAL_NEAR_MIN(IntArgs *n);
  IntValBranch* INT_VAL_NEAR_MAX(IntArgs *n);
  IntValBranch* INT_VAL_NEAR_INC(IntArgs *n);
  IntValBranch* INT_VAL_NEAR_DEC(IntArgs *n);

  /* search engines */
  DFS<CLSpace> *gecode_dfs_engine_create(CLSpace *space);
  void gecode_dfs_engine_delete(DFS<CLSpace> *dfs);
  void gecode_dfs_engine_delete_sap(void *dfs);
  CLSpace *gecode_dfs_engine_next(DFS<CLSpace> *dfs);
  BAB<CLSpace> *gecode_bab_engine_create(CLSpace *space, size_t minVar);
  void gecode_bab_engine_delete(BAB<CLSpace> *bab);
  void gecode_bab_engine_delete_sap(void *bab);
  CLSpace *gecode_bab_engine_next(BAB<CLSpace> *bab);

  RBS<DFS, CLSpace> *gecode_rbs_dfs_engine_create(CLSpace *space);
  void gecode_rbs_dfs_engine_delete(RBS<DFS, CLSpace> *rbs);
  void gecode_rbs_dfs_engine_delete_sap(void *rbs);
  CLSpace *gecode_rbs_dfs_engine_next(RBS<DFS, CLSpace> *rbs);

  RBS<BAB, CLSpace> *gecode_rbs_bab_engine_create(CLSpace *space, size_t minVar);
  void gecode_rbs_bab_engine_delete(RBS<BAB, CLSpace> *rbs);
  void gecode_rbs_bab_engine_delete_sap(void *rbs);
  CLSpace *gecode_rbs_bab_engine_next(RBS<BAB, CLSpace> *rbs);
  

  /* ### propagator interfaces ### */

  /* FD Boolean variable relations */
  void gecode_rel_bvar_int(CLSpace *space, IntRelType op,
                           BoolVar* v, int value, IntConLevel icl);

  void gecode_rel_bvar_bvar(CLSpace *space, IntRelType op,
                            BoolVar* v1, BoolVar* v2, IntConLevel icl);

  void gecode_rel_bvar_int_reified(CLSpace *space, IntRelType op, BoolVar* v1,
                                   int val, ReifyMode mode, BoolVar* v2, IntConLevel icl);
  
  void gecode_rel_bvar_bvar_reified(CLSpace *space, IntRelType op, BoolVar* v1,
                                    BoolVar* v2, ReifyMode mode, BoolVar* b1, IntConLevel icl);

  void gecode_rel_bvars(CLSpace *space, IntRelType op, BoolVarArgs* v,
                        IntConLevel icl);

  void gecode_rel_bvars_int(CLSpace *space, IntRelType op, BoolVarArgs* v,
                            int value, IntConLevel icl);

  void gecode_rel_bvars_bvars(CLSpace *space, IntRelType op,
                              BoolVarArgs* v1, BoolVarArgs* v2,
                              IntConLevel icl);


  /* Boolean operations */
  void gecode_op_bvar_bvar_bvar(CLSpace *space, BoolOpType op, BoolVar* v1,
                                BoolVar* v2, BoolVar* b1, IntConLevel icl);

  void gecode_op_bvar_bvar_int(CLSpace *space, BoolOpType op, BoolVar* v1,
                               BoolVar* v2, int n, IntConLevel icl);

  void gecode_op_bvars_int(CLSpace *space, BoolOpType op, BoolVarArgs* v,
                           int value, IntConLevel icl);

  void gecode_op_bvars_bvar(CLSpace *space, BoolOpType op, BoolVarArgs* v,
                            BoolVar* var, IntConLevel icl);

  void gecode_clause_bvars_bvars_int(CLSpace *space, BoolOpType op,
                                     BoolVarArgs* v1, BoolVarArgs* v2,
                                     int n, IntConLevel icl);

  void gecode_clause_bvars_bvars_bvar(CLSpace *space, BoolOpType op,
                                      BoolVarArgs* v1, BoolVarArgs* v2,
                                      BoolVar* bvar, IntConLevel icl);

  void gecode_ite_bvar_ivar_ivar_ivar(CLSpace *space, BoolVar* bvar,
                                      IntVar* x, IntVar* y, IntVar* z);


  /* integer relations */
  void gecode_dom_ivar_int(CLSpace *space, IntVar* v, int value, IntConLevel icl);

  void gecode_dom_ivars_int(CLSpace *space, IntVarArgs* v, int value, IntConLevel icl);

  void gecode_dom_ivar_int_int(CLSpace *space, IntVar* v, int l, int m, IntConLevel icl);

  void gecode_dom_ivars_int_int(CLSpace *space, IntVarArgs* v,
                                int l, int m, IntConLevel icl);

  void gecode_dom_ivar_iset(CLSpace *space, IntVar* v,  IntSet* s, IntConLevel icl);

  void gecode_dom_ivars_iset(CLSpace *space, IntVarArgs* v, IntSet* s, IntConLevel icl);

  void gecode_dom_ivar_int_reify(CLSpace *space, IntVar* v, int value,
                                 ReifyMode mode, BoolVar* bvar, IntConLevel icl);

  void gecode_dom_ivar_int_int_reify(CLSpace *space, IntVar* v, int l, int m,
                                     ReifyMode mode, BoolVar* bvar, IntConLevel icl);

  void gecode_dom_ivar_iset_reify(CLSpace *space, IntVar* v, IntSet* s,
                                  ReifyMode mode, BoolVar* bvar, IntConLevel icl);

  void gecode_dom_ivar_ivar(CLSpace *space, IntVar* v, IntVar* b, IntConLevel icl);

  void gecode_dom_bvar_bvar(CLSpace *space, BoolVar* v, BoolVar* b, IntConLevel icl);

  void gecode_dom_ivars_ivars(CLSpace *space, IntVarArgs* v, IntVarArgs* b,
                              IntConLevel icl);

  void gecode_dom_bvars_bvars(CLSpace *space, BoolVarArgs* v, BoolVarArgs* b,
                              IntConLevel icl);

  void gecode_rel_ivar_int(CLSpace *space, IntRelType op,
                           IntVar* v, int value, IntConLevel icl);

  void gecode_rel_ivar_ivar(CLSpace *space, IntRelType op,
                            IntVar* v1, IntVar* v2, IntConLevel icl);

  void gecode_rel_ivar_int_reified(CLSpace *space, IntRelType op, IntVar* v1,
                                   int val, ReifyMode mode, BoolVar* b1, IntConLevel icl);

  void gecode_rel_ivar_ivar_reified(CLSpace *space, IntRelType op, IntVar* v1,
                                    IntVar* v2, ReifyMode mode, BoolVar* b1, IntConLevel icl);

  void gecode_rel_ivars(CLSpace *space, IntRelType op, IntVarArgs* v,
                        IntConLevel icl);

  void gecode_rel_ivars_int(CLSpace *space, IntRelType op, IntVarArgs* v,
                            int value, IntConLevel icl);

  void gecode_rel_ivars_ivars(CLSpace *space, IntRelType op,
                              IntVarArgs* v1, IntVarArgs* v2,
                              IntConLevel icl);


  /* distinct constraint */
  void gecode_distinct_ivars(CLSpace *space, IntVarArgs *va, IntConLevel icl);
  void gecode_distinct_ints_ivars(CLSpace *space, IntArgs *ia, IntVarArgs *va, IntConLevel icl);


  /* sorted constraint */
  void gecode_sorted_ivars_ivars(CLSpace *space, IntVarArgs* xvars, 
                                 IntVarArgs* yvars, IntConLevel icl);

  void gecode_sorted_ivars_ivars_ivars(CLSpace *space, IntVarArgs* xvars,
                                       IntVarArgs* yvars, IntVarArgs* zvars,
                                       IntConLevel icl);


  /* count constraint */
  void gecode_count_rel_ivars_int_int(CLSpace *space, IntVarArgs* x,
                                      int n, IntRelType irt, int m,
                                      IntConLevel icl);

  void gecode_count_rel_ivars_iset_int(CLSpace *space, IntVarArgs* x,
                                       IntSet* y, IntRelType irt, int m,
                                       IntConLevel icl);

  void gecode_count_rel_ivars_ivar_int(CLSpace *space, IntVarArgs* x,
                                       IntVar* y, IntRelType irt, int m,
                                       IntConLevel icl);

  void gecode_count_rel_ivars_ivars_int(CLSpace *space, IntVarArgs* x,
                                        IntArgs* y, IntRelType irt, int m,
                                        IntConLevel icl);

  void gecode_count_rel_ivars_int_ivar(CLSpace *space, IntVarArgs* x,
                                       int n, IntRelType irt, IntVar* z,
                                       IntConLevel icl);

  void gecode_count_rel_ivars_iset_ivar(CLSpace *space, IntVarArgs* x,
                                        IntSet* y, IntRelType irt, IntVar* z,
                                        IntConLevel icl);

  void gecode_count_rel_ivars_ivar_ivar(CLSpace *space, IntVarArgs* x,
                                        IntVar* y, IntRelType irt, IntVar* z,
                                        IntConLevel icl);

  void gecode_count_ivars_ivars(CLSpace *space, IntVarArgs* x,
                                IntVarArgs* c, IntConLevel icl);

  void gecode_count_ivars_isets(CLSpace *space, IntVarArgs* x,
                                IntSetArgs* c, IntConLevel icl);

  void gecode_count_ivars_ivars_ints(CLSpace *space, IntVarArgs* x,
                                     IntVarArgs* c, IntArgs* v, IntConLevel icl);

  void gecode_count_ivars_isets_ints(CLSpace *space, IntVarArgs* x,
                                     IntSetArgs* c, IntArgs* v, IntConLevel icl);

  void gecode_count_ivars_iset_ints(CLSpace *space, IntVarArgs* x,
                                    IntSet* c, IntArgs* v, IntConLevel icl);


  /* nvalues constraint */
  void gecode_nvalues_ivars_int(CLSpace *space, IntRelType irt,
                                IntVarArgs* x, int y,
                                IntConLevel icl);

  void gecode_nvalues_ivars_ivar(CLSpace *space, IntRelType irt,
                                 IntVarArgs* x, IntVar* y,
                                 IntConLevel icl);

  void gecode_nvalues_bvars_int(CLSpace *space, IntRelType irt,
                                BoolVarArgs* x, int y,
                                IntConLevel icl);

  void gecode_nvalues_bvars_ivar(CLSpace *space, IntRelType irt,
                                 BoolVarArgs* x, IntVar* y,
                                 IntConLevel icl);

  
  /* sequence constraint */
  void gecode_sequence_ivars(CLSpace *space, IntVarArgs* x, IntSet* s,
                             int q, int l, int u, IntConLevel icl);

  void gecode_sequence_bvars(CLSpace *space, BoolVarArgs* x, IntSet* s,
                             int q, int l, int u, IntConLevel icl);


  /* binpacking constraint */
  void gecode_binpacking(CLSpace *space,
                         IntVarArgs* l, IntVarArgs *b, IntArgs* s,
                         IntConLevel icl);

  /* nooverlap constraint */
  void gecode_nooverlap(CLSpace *space,
                        IntVarArgs *x, IntArgs* w,
                        IntVarArgs *y, IntArgs* h,
                        IntConLevel icl);
  void gecode_nooverlap_optional(CLSpace *space,
                                 IntVarArgs *x, IntArgs* w,
                                 IntVarArgs *y, IntArgs* h,
                                 BoolVarArgs* o,
                                 IntConLevel icl);
  void gecode_nooverlap_coords(CLSpace *space,
                               IntVarArgs *x0, IntVarArgs* w, IntVarArgs *x1,
                               IntVarArgs *y0, IntVarArgs* h, IntVarArgs *y1,
                               IntConLevel icl);
  void gecode_nooverlap_coords_optional(CLSpace *space,
                                        IntVarArgs *x0, IntVarArgs* w, IntVarArgs *x1,
                                        IntVarArgs *y0, IntVarArgs* h, IntVarArgs *y1,
                                        BoolVarArgs* o,
                                        IntConLevel icl);


  /* DFA / extensional constraint */
  DFA* gecode_DFA_create(int s, DFA::Transition* trns, int* f);
  void gecode_DFA_delete(DFA* d);
  void gecode_DFA_delete_sap(void* d);

  void gecode_extensional_ivars_dfa(CLSpace* space, IntVarArgs* x, DFA* d,
                                    IntConLevel icl);

  void gecode_extensional_bvars_dfa(CLSpace* space, BoolVarArgs* x, DFA* d,
                                    IntConLevel icl);

  TupleSet* gecode_TupleSet_create(void);
  void gecode_TupleSet_delete(TupleSet* d);
  void gecode_TupleSet_delete_sap(void* d);

  void gecode_TupleSet_add(TupleSet* d, IntArgs* i);

  int gecode_TupleSet_count(TupleSet* d);

  void gecode_extensional_ivars_tset(CLSpace* space, IntVarArgs* x, TupleSet* d,
                                     ExtensionalPropKind epk, IntConLevel icl);

  void gecode_extensional_bvars_tset(CLSpace* space, BoolVarArgs* x, TupleSet* d,
                                     ExtensionalPropKind epk, IntConLevel icl);


  /* cumulatives constraint */
  void gecode_cumulatives_ivars(CLSpace *space,
                                IntVarArgs* m,
                                IntVarArgs* s,
                                IntVarArgs* p,
                                IntVarArgs* e,
                                IntVarArgs* u,
                                IntArgs* c,
                                bool at_most,
                                IntConLevel icl);

  /* unary constraint */
  void gecode_unary_ivars(CLSpace *space, IntVarArgs* s, IntArgs* p,
                          IntConLevel icl);

  void gecode_unary_ivars_bvars(CLSpace *space, IntVarArgs* s, IntArgs* p,
                                BoolVarArgs* b, IntConLevel icl);

  void gecode_unary_tasks_ivars(CLSpace *space, TaskTypeArgs* tasks, 
                                IntVarArgs* flex, IntArgs* fix,
                                IntConLevel icl);

  void gecode_unary_tasks_ivars_bvars(CLSpace *space, TaskTypeArgs* tasks, 
                                      IntVarArgs* flex, IntArgs* fix,
                                      BoolVarArgs* m, IntConLevel icl);

  void gecode_unary_ivars_ivars(CLSpace *space, IntVarArgs* s,
                                IntVarArgs* p, IntVarArgs* e,
                                IntConLevel icl);

  void gecode_unary_ivars_ivars_bvars(CLSpace *space, IntVarArgs* s,
                                      IntVarArgs* p, IntVarArgs* e,
                                      BoolVarArgs* m, IntConLevel icl);


  /* cumulative constraint */
  void gecode_cumulative_int_tasks(CLSpace *space, int c,
                                   TaskTypeArgs* t, IntVarArgs* flex,
                                   IntArgs* fix, IntArgs* u,
                                   IntConLevel icl);

  void gecode_cumulative_ivar_tasks(CLSpace *space, IntVar* c,
                                    TaskTypeArgs* t, IntVarArgs* flex,
                                    IntArgs* fix, IntArgs* u,
                                    IntConLevel icl);

  void gecode_cumulative_int_tasks_bvars(CLSpace *space, int c,
                                         TaskTypeArgs* t, IntVarArgs* flex,
                                         IntArgs* fix, IntArgs* u, BoolVarArgs* m,
                                         IntConLevel icl);

  void gecode_cumulative_ivar_tasks_bvars(CLSpace *space, IntVar* c,
                                          TaskTypeArgs* t, IntVarArgs* flex,
                                          IntArgs* fix, IntArgs* u, BoolVarArgs* m,
                                          IntConLevel icl);

  void gecode_cumulative_int(CLSpace *space, int c,
                             IntVarArgs* s, IntArgs* p, IntArgs* u,
                             IntConLevel icl);

  void gecode_cumulative_ivar(CLSpace *space, IntVar* c,
                              IntVarArgs* s, IntArgs* p, IntArgs* u,
                              IntConLevel icl);

  void gecode_cumulative_int_bvars(CLSpace *space, int c,
                                   IntVarArgs* s, IntArgs* p, IntArgs* u,
                                   BoolVarArgs* m, IntConLevel icl);

  void gecode_cumulative_ivar_bvars(CLSpace *space, IntVar* c,
                                    IntVarArgs* s, IntArgs* p, IntArgs* u,
                                    BoolVarArgs* m, IntConLevel icl);

  void gecode_cumulative_int_ivars(CLSpace *space, int c,
                                   IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                   IntArgs* u, IntConLevel icl);

  void gecode_cumulative_ivar_ivars(CLSpace *space, IntVar* c,
                                    IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                    IntArgs* u, IntConLevel icl);

  void gecode_cumulative_int_ivars_bvars(CLSpace *space, int c,
                                         IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                         IntArgs* u, BoolVarArgs* m, IntConLevel icl);

  void gecode_cumulative_ivar_ivars_bvars(CLSpace *space, IntVar* c,
                                          IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                          IntArgs* u, BoolVarArgs* m, IntConLevel icl);


  /* circuit constraint */
  void gecode_circuit_ivars_costs(CLSpace *space, IntArgs* c, IntVarArgs* x,
                                  IntVarArgs* y, IntVar* z, IntConLevel icl);

  void gecode_circuit_ivars_costs_offset(CLSpace *space, IntArgs* c, int offset, 
                                         IntVarArgs* x, IntVarArgs* y, IntVar* z,
                                         IntConLevel icl);

  void gecode_circuit_ivars(CLSpace *space, IntArgs* c, IntVarArgs* x,
                            IntVar* z, IntConLevel icl);

  void gecode_circuit_ivars_offset(CLSpace *space, IntArgs* c, int offset, 
                                   IntVarArgs* x, IntVar* z, IntConLevel icl);
  

  /* Hamiltonian path constraint */
  void gecode_path_ivars(CLSpace *space, IntVarArgs* x,
                         IntVar* s, IntVar* e, IntConLevel icl);

  void gecode_path_ivars_offset(CLSpace *space, int offset, IntVarArgs* x,
                                IntVar* s, IntVar* e, IntConLevel icl);

  void gecode_path_ivars_edge_costs(CLSpace *space, IntArgs* c, IntVarArgs* x,
                                    IntVar* s, IntVar* e, IntVarArgs* y,
                                    IntVar* z, IntConLevel icl);

  void gecode_path_ivars_edge_costs_offset(CLSpace *space, IntArgs* c, int offset,
                                           IntVarArgs* x, IntVar* s, IntVar* e,
                                           IntVarArgs* y, IntVar* z, IntConLevel icl);

  void gecode_path_ivars_costs(CLSpace *space, IntArgs* c, IntVarArgs* x,
                               IntVar* s, IntVar* e, IntVar* z,
                               IntConLevel icl);

  void gecode_path_ivars_costs_offset(CLSpace *space, IntArgs* c, int offset,
                                      IntVarArgs* x, IntVar* s, IntVar* e,
                                      IntVar* z, IntConLevel icl);


  /* channeling constraints */
  void gecode_channel_ivars_ivars(CLSpace *space, IntVarArgs* v1,
                                  IntVarArgs* v2, IntConLevel icl);

  void gecode_channel_ivars_int_ivars_int(CLSpace *space,
                                          IntVarArgs* v1, int xoff,
                                          IntVarArgs* v2, int yoff,
                                          IntConLevel icl);

  void gecode_channel_bvar_ivar(CLSpace *space,
                                BoolVar* bvar, IntVar* ivar,
                                IntConLevel icl);

  void gecode_channel_ivar_bvar(CLSpace *space,
                                IntVar* ivar, BoolVar* bvar,
                                IntConLevel icl);

  void gecode_channel_bvars_ivar_int(CLSpace *space,
                                     BoolVarArgs* v1,
                                     IntVar* ivar, int offset,
                                     IntConLevel icl);


  /* integer arithmetic constraints */
  void gecode_min_ivar_ivar_ivar(CLSpace *space,
                                 IntVar* x1, IntVar* x2,
                                 IntVar* ret, IntConLevel icl);

  void gecode_min_ivars_ivar(CLSpace *space,
                             IntVarArgs* v, IntVar* y,
                             IntConLevel icl);

  void gecode_max_ivar_ivar_ivar(CLSpace *space,
                                 IntVar* x1, IntVar* x2,
                                 IntVar* ret, IntConLevel icl);

  void gecode_max_ivars_ivar(CLSpace *space,
                             IntVarArgs* v, IntVar* y,
                             IntConLevel icl);

  void gecode_abs_ivar_ivar(CLSpace *space,
                            IntVar* x1, IntVar* x2,
                            IntConLevel icl);

  void gecode_mul_ivar_ivar_ivar(CLSpace *space,
                                 IntVar* x1, IntVar* x2,
                                 IntVar* ret, IntConLevel icl);

  void gecode_sqr_ivar_ivar(CLSpace *space,
                            IntVar* x1, IntVar* x2,
                            IntConLevel icl);

  void gecode_sqrt_ivar_ivar(CLSpace *space,
                             IntVar* x1, IntVar* x2,
                             IntConLevel icl);

  void gecode_divmod_ivar_ivar_ivar_ivar(CLSpace *space,
                                         IntVar* x0, IntVar* x1,
                                         IntVar* x2, IntVar* x3,
                                         IntConLevel icl);

  void gecode_div_ivar_ivar_ivar(CLSpace *space,
                                 IntVar* x1, IntVar* x2,
                                 IntVar* ret, IntConLevel icl);

  void gecode_mod_ivar_ivar_ivar(CLSpace *space,
                                 IntVar* x1, IntVar* x2,
                                 IntVar* ret, IntConLevel icl);


  /* integer linear constraint */
  void gecode_lin_ivars_int(CLSpace *space, IntRelType rel, IntVarArgs* v,
                            int value, IntConLevel icl);

  void gecode_lin_ivars_ivar(CLSpace *space, IntRelType rel, IntVarArgs* v,
                             IntVar* var, IntConLevel icl);

  void gecode_lin_ivars_int_reified(CLSpace *space, IntRelType rel, IntVarArgs* v,
                                    int value, ReifyMode mode,
                                    BoolVar* bvar, IntConLevel icl);

  void gecode_lin_ivars_ivar_reified(CLSpace *space, IntRelType rel, IntVarArgs* v,
                                     IntVar* var, ReifyMode mode, 
                                     BoolVar* bvar, IntConLevel icl);

  void gecode_lin_ints_ivars_int(CLSpace *space, IntRelType rel, IntArgs* ints,
                                 IntVarArgs* v, int value, IntConLevel icl);

  void gecode_lin_ints_ivars_ivar(CLSpace *space, IntRelType rel,IntArgs* ints,
                                  IntVarArgs* v, IntVar* var, IntConLevel icl);

  void gecode_lin_ints_ivars_int_reified(CLSpace *space, IntRelType rel, IntArgs* ints,
                                         IntVarArgs* v, int value, 
                                         ReifyMode mode, BoolVar* bvar,
                                         IntConLevel icl);

  void gecode_lin_ints_ivars_ivar_reified(CLSpace *space, IntRelType rel,
                                          IntArgs* ints, IntVarArgs* v,
                                          IntVar* var, ReifyMode mode, 
                                          BoolVar* bvar, IntConLevel icl);


  /* boolean linear constraint */
  void gecode_lin_bvars_int(CLSpace *space, IntRelType rel, BoolVarArgs* v,
                            int value, IntConLevel icl);

  void gecode_lin_bvars_ivar(CLSpace *space, IntRelType rel, BoolVarArgs* v,
                             IntVar* var, IntConLevel icl);

  void gecode_lin_bvars_int_reified(CLSpace *space, IntRelType rel, 
                                    BoolVarArgs* v, int value, ReifyMode mode,
                                    BoolVar* bvar, IntConLevel icl);

  void gecode_lin_bvars_ivar_reified(CLSpace *space, IntRelType rel,
                                     BoolVarArgs* v, IntVar* var, ReifyMode mode,
                                     BoolVar* bvar, IntConLevel icl);

  void gecode_lin_ints_bvars_int(CLSpace *space, IntRelType rel, 
                                 IntArgs* ints, BoolVarArgs* v,
                                 int value, IntConLevel icl);

  void gecode_lin_ints_bvars_ivar(CLSpace *space, IntRelType rel, 
                                  IntArgs* ints, BoolVarArgs* v,
                                  IntVar* var, IntConLevel icl);

  void gecode_lin_ints_bvars_int_reified(CLSpace *space, IntRelType rel,
                                         IntArgs* ints, BoolVarArgs* v,
                                         int value, ReifyMode mode, 
                                         BoolVar* bvar, IntConLevel icl);

  void gecode_lin_ints_bvars_ivar_reified(CLSpace *space, IntRelType rel,
                                          IntArgs* ints, BoolVarArgs* v,
                                          IntVar* var, ReifyMode mode, 
                                          BoolVar* bvar, IntConLevel icl);


  /* float domain specific functions */
  void gecode_dom_fvar_dbl(CLSpace *space, FloatVar* x, double n);
  void gecode_dom_fvars_dbl(CLSpace *space, FloatVarArgs* x, double n);
  void gecode_dom_fvar_dbl_dbl(CLSpace *space, FloatVar* x, double l, double m);
  void gecode_dom_fvars_dbl_dbl(CLSpace *space, FloatVarArgs* x,
                                double l, double u);
  void gecode_dom_fvar_dbl_reified(CLSpace *space, FloatVar* x, double n,
                                   ReifyMode mode, BoolVar* bvar);
  void gecode_dom_fvar_dbl_dbl_reified(CLSpace *space, FloatVar* x,
                                       double l, double m,
                                       ReifyMode mode, BoolVar* bvar);
  void gecode_dom_fvar_fvar(CLSpace *space, FloatVar* x, FloatVar* d);
  void gecode_dom_fvars_fvars(CLSpace *space, FloatVarArgs* x, FloatVarArgs* d);

  void gecode_rel_fvar_fvar(CLSpace *space, FloatRelType op, FloatVar* x0, FloatVar* x1);
  void gecode_rel_fvar_dbl(CLSpace *space, FloatRelType op, FloatVar* x0, double x1);
  void gecode_rel_fvar_dbl_reified(CLSpace *space, FloatRelType op, FloatVar* x0, double x1,
				   ReifyMode mode, BoolVar* bvar);
  void gecode_rel_fvar_fvar_reified(CLSpace *space, FloatRelType op, FloatVar* x0, FloatVar* x1,
				    ReifyMode mode, BoolVar* bvar);
  void gecode_rel_fvars_dbl(CLSpace *space, FloatRelType op, FloatVarArgs* x0, double x1);
  void gecode_rel_fvars_fvar(CLSpace *space, FloatRelType op, FloatVarArgs* x0, FloatVar* x1);
  void gecode_min_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2);
  void gecode_min_fvars_fvar(CLSpace *space, FloatVarArgs* x, FloatVar* y);
  void gecode_max_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2);
  void gecode_max_fvars_fvar(CLSpace *space, FloatVarArgs* x, FloatVar* y);
  void gecode_abs_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_mult_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2);
  void gecode_sqr_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_sqrt_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_pow_fvar_uint_fvar(CLSpace *space, FloatVar* x0, int pow, FloatVar* x1);
  void gecode_nroot_fvar_uint_fvar(CLSpace *space, FloatVar* x0, int pow, FloatVar* x1);
  void gecode_div_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2);
  void gecode_exp_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_log_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_exp_dbl_fvar_fvar(CLSpace *space, double base, FloatVar* x0, FloatVar* x1);
  void gecode_log_dbl_fvar_fvar(CLSpace *space, double base, FloatVar* x0, FloatVar* x1);
  void gecode_asin_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_sin_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_acos_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_cos_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_atan_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);
  void gecode_tan_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1);

  void gecode_lin_fvars_dbl(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
			    double c);
  void gecode_lin_fvars_fvar(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
			     FloatVar* y);
  void gecode_lin_fvars_dbl_reified(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
				    double c, ReifyMode mode, BoolVar* bvar);
  void gecode_lin_fvars_fvar_reified(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
				     FloatVar* y, ReifyMode mode, BoolVar* bvar);
  void gecode_lin_fargs_fvars_dbl(CLSpace *space, FloatRelType rel, FloatValArgs* a, FloatVarArgs* x,
				  double c);
  void gecode_lin_fargs_fvars_fvar(CLSpace *space, FloatRelType rel, FloatValArgs* a, FloatVarArgs* x,
				   FloatVar* y);
  void gecode_lin_fargs_fvars_dbl_reified(CLSpace *space, FloatRelType rel, FloatValArgs* a,
					  FloatVarArgs* x, double c,
					  ReifyMode mode, BoolVar* bvar);
  void gecode_lin_fargs_fvars_fvar_reified(CLSpace *space, FloatRelType rel, FloatValArgs* a,
					   FloatVarArgs* x, FloatVar* y,
					   ReifyMode mode, BoolVar* bvar);

  void gecode_channel_fvar_ivar(CLSpace *space, FloatVar* x0, IntVar* x1);

  // Float Branchers
  BrancherHandle* gecode_branch_fvar(CLSpace *space, FloatVar* var, FloatValBranch* valb);
  BrancherHandle* gecode_branch_fvars(CLSpace *space, FloatVarArgs* vars,
                                      FloatVarBranch* varb, FloatValBranch* valb);
  BrancherHandle* gecode_branch_fvars_tie(CLSpace *space, FloatVarArgs* vars,
                                          FloatVarBranch** varb, unsigned int cnt,
                                          FloatValBranch* valb);


  void gecode_fvar_selector_delete(FloatVarBranch* s);
  void gecode_fvar_selector_delete_sap(void* s);

  FloatVarBranch* FLOAT_VAR_NONE(void);
  FloatVarBranch* FLOAT_VAR_RND(unsigned int seed);
  /* TODO: check whether to insert MERIT variants */
  FloatVarBranch* FLOAT_VAR_DEGREE_MIN(void);
  FloatVarBranch* FLOAT_VAR_DEGREE_MAX(void);
  FloatVarBranch* FLOAT_VAR_AFC_MIN(void);
  FloatVarBranch* FLOAT_VAR_AFC_MAX(void);
  /* TODO: check whether to insert ACTIVITY variants */
  FloatVarBranch* FLOAT_VAR_MIN_MIN(void);
  FloatVarBranch* FLOAT_VAR_MIN_MAX(void);
  FloatVarBranch* FLOAT_VAR_MAX_MIN(void);
  FloatVarBranch* FLOAT_VAR_MAX_MAX(void);
  FloatVarBranch* FLOAT_VAR_SIZE_MIN(void);
  FloatVarBranch* FLOAT_VAR_SIZE_MAX(void);
  FloatVarBranch* FLOAT_VAR_DEGREE_SIZE_MIN(void);
  FloatVarBranch* FLOAT_VAR_DEGREE_SIZE_MAX(void);
  FloatVarBranch* FLOAT_VAR_AFC_SIZE_MIN(void);
  FloatVarBranch* FLOAT_VAR_AFC_SIZE_MAX(void);
  /* TODO: check whether to insert ACTIVITY variants */

  void gecode_fval_selector_delete(FloatValBranch* s);
  FloatValBranch* FLOAT_VAL_SPLIT_MIN(void);
  FloatValBranch* FLOAT_VAL_SPLIT_MAX(void);
  FloatValBranch* FLOAT_VAL_SPLIT_RND(unsigned int seed);

  /* finite (integer) sets */
  IntSet* gecode_intset_bounds(int min, int max);
  IntSet* gecode_intset_seq(int seq[], int count);
  IntSet* gecode_intset_ranges(int seq[][2], int count);
  void gecode_intset_delete(IntSet* iset);
  void gecode_intset_delete_sap(void* iset);
  /* set domain constraints */
  void gecode_dom_svar_int(CLSpace *space, SetRelType r, SetVar* x, int i);
  void gecode_dom_svars_int(CLSpace *space, SetRelType r, SetVarArgs* x, int i);
  void gecode_dom_svar_int_int(CLSpace *space, SetRelType r, SetVar* x,
                               int i, int j);
  void gecode_dom_svars_int_int(CLSpace *space, SetRelType r, SetVarArgs* x,
                                int i, int j);
  void gecode_dom_svar_iset(CLSpace *space, SetRelType r, SetVar* x, IntSet* s);
  void gecode_dom_svars_iset(CLSpace *space, SetRelType r, SetVarArgs* x, IntSet* s);

  void gecode_cardinality_svar_uint_uint(CLSpace *space, SetVar* x,
                                         unsigned int i, unsigned int j);
  void gecode_cardinality_svars_uint_uint(CLSpace *space, SetVarArgs* x,
                                          unsigned int i, unsigned int j);
  void gecode_cardinality_svar_ivar(CLSpace *space, SetVar* x, IntVar* i);

  void gecode_dom_svar_int_reified(CLSpace *space, SetRelType r, SetVar* x,
                                   int i, ReifyMode mode, BoolVar* b);
  void gecode_dom_svar_int_int_reified(CLSpace *space, SetRelType r, SetVar* x,
                                       int i, int j, ReifyMode mode, BoolVar* b);
  void gecode_dom_svar_iset_reified(CLSpace *space, SetRelType r, SetVar* x,
                                    IntSet* i, ReifyMode mode, BoolVar* b);

  void gecode_dom_svar_svar(CLSpace *space, SetVar* x, SetVar* d);
  void gecode_dom_svars_svars(CLSpace *space, SetVarArgs* x, SetVarArgs* d);

  /* set relations */
  void gecode_srel_svar_svar(CLSpace *space, SetVar* x, SetRelType r, SetVar* y);
  void gecode_srel_svar_svar_reified(CLSpace *space, SetVar* x, SetRelType r, SetVar* y,
                                     ReifyMode mode, BoolVar* b);
  void gecode_srel_svar_ivar(CLSpace *space, SetVar* x, SetRelType r, IntVar* y);
  void gecode_srel_ivar_svar(CLSpace *space, IntVar* x, SetRelType r, SetVar* y);
  void gecode_srel_svar_ivar_reified(CLSpace *space, SetVar* x, SetRelType r, IntVar* y,
                                     ReifyMode mode, BoolVar* b);
  void gecode_srel_ivar_svar_reified(CLSpace *space, IntVar* x, SetRelType r, SetVar* y,
                                     ReifyMode mode, BoolVar* b);
  void gecode_irel_svar_ivar(CLSpace *space, SetVar* x, IntRelType r, IntVar* y);
  void gecode_irel_ivar_svar(CLSpace *space, IntVar* x, IntRelType r, SetVar* y);
  /* set operations */
  void gecode_sop_svar_svar_srel_svar(CLSpace *space, SetVar* x, SetOpType op, SetVar* y,
                                      SetRelType r, SetVar* z);
  void gecode_sop_svars_eql_svar(CLSpace *space, SetOpType op, SetVarArgs* x,
                                 SetVar* y);
  void gecode_sop_svars_iset_eql_svar(CLSpace *space, SetOpType op, SetVarArgs* x,
                                      IntSet* z, SetVar* y);
  void gecode_sop_ivars_iset_eql_svar(CLSpace *space, SetOpType op, IntVarArgs* x,
                                      IntSet* z, SetVar* y);
  void gecode_sop_ivars_eql_svar(CLSpace *space, SetOpType op, IntVarArgs* x,
                                 SetVar* y);
  void gecode_sop_iset_svar_srel_svar(CLSpace *space, IntSet* x, SetOpType op, SetVar* y,
                                      SetRelType r, SetVar* z);
  void gecode_sop_svar_iset_srel_svar(CLSpace *space, SetVar* x, SetOpType op, IntSet* y,
                                      SetRelType r, SetVar* z);
  void gecode_sop_svar_svar_srel_iset(CLSpace *space, SetVar* x, SetOpType op, SetVar* y,
                                      SetRelType r, IntSet* z);
  void gecode_sop_iset_svar_srel_iset(CLSpace *space, IntSet* x, SetOpType op, SetVar* y,
                                      SetRelType r, IntSet* z);
  void gecode_sop_svar_iset_srel_iset(CLSpace *space, SetVar* x, SetOpType op, IntSet* y,
                                      SetRelType r, IntSet* z);
  /* convex hull constraint */
  void gecode_convex_svar(CLSpace *space, SetVar* x);
  void gecode_convex_svar_svar(CLSpace *space, SetVar* x, SetVar* y);
  /* sequence constraint */
  void gecode_sequence_svars(CLSpace *space, SetVarArgs* x);
  void gecode_sequence_svars_svar(CLSpace *space, SetVarArgs* y, SetVar* x);
  /* distinctness constraint*/
  void gecode_atmostOne_svars(CLSpace *space, SetVarArgs* x, unsigned int c);
  /* connection constraints */
  void gecode_min_svar_ivar(CLSpace *space, SetVar* x, IntVar* i);
  void gecode_notMin_svar_ivar(CLSpace *space, SetVar* x, IntVar* i);
  void gecode_min_svar_ivar_reified(CLSpace *space, SetVar* x, IntVar* i,
                                    ReifyMode mode, BoolVar* b);
  void gecode_max_svar_ivar(CLSpace *space, SetVar* x, IntVar* i);
  void gecode_notMax_svar_ivar(CLSpace *space, SetVar* x, IntVar* i);
  void gecode_max_svar_ivar_reified(CLSpace *space, SetVar* x, IntVar* i,
                                    ReifyMode mode, BoolVar* b);
  void gecode_weights_ints_ints_svar_ivar(CLSpace *space, IntArgs* elements,
                                          IntArgs* weights, SetVar* x, IntVar* y);
  /* channel constraint */
  void gecode_channel_ivars_svars(CLSpace *space, IntVarArgs* x, SetVarArgs* y);
  void gecode_channel_bvars_svar(CLSpace *space, BoolVarArgs* x, SetVar* y);
  void gecode_channel_svars_svars(CLSpace *space, SetVarArgs* x, SetVarArgs* y);
  void gecode_channelSorted_ivars_svar(CLSpace *space, IntVarArgs* x, SetVar* y);
  /* precede constraint */
  void gecode_precede_svars_int_int(CLSpace *space, SetVarArgs* x, int s, int t);
  void gecode_precede_svars_ints(CLSpace *space, SetVarArgs* x, IntArgs* c);
  /* element constraint */
  void gecode_element_svars_svar_svar(CLSpace *space, SetOpType op, SetVarArgs* x,
                                      SetVar* y, SetVar* z);
  void gecode_element_ivars_svar_svar(CLSpace *space, SetOpType op, IntVarArgs* x,
                                      SetVar* y, SetVar* z);
  void gecode_element_isets_svar_svar(CLSpace *space, SetOpType op, IntSetArgs* x,
                                      SetVar* y, SetVar* z);
  void gecode_element_ints_svar_svar(CLSpace *space, SetOpType op, IntArgs* x,
                                     SetVar* y, SetVar* z);
  void gecode_element_svars_ivar_svar(CLSpace *space,
                                      SetVarArgs* x, IntVar* y, SetVar* z);
  void gecode_element_isets_ivar_svar(CLSpace *space,
                                      IntSetArgs* x, IntVar* y, SetVar* z);
  void gecode_element_isets_ivar_int_ivar_int_svar(CLSpace *space, IntSetArgs* a, 
                                                   IntVar* x, int w,
                                                   IntVar* y, int h,
                                                   SetVar* z);
  void gecode_element_svars_ivar_int_ivar_int_svar(CLSpace *space, SetVarArgs* a, 
                                                   IntVar* x, int w,
                                                   IntVar* y, int h,
                                                   SetVar* z);

  /* set branchers */
  BrancherHandle* gecode_branch_svar(CLSpace *space, SetVar* var, SetValBranch* valb);
  BrancherHandle* gecode_branch_svars(CLSpace *space, SetVarArgs* vars,
                                      SetVarBranch* varb, SetValBranch* valb);
  BrancherHandle* gecode_branch_svars_tie(CLSpace *space, SetVarArgs* vars,
                                          SetVarBranch** varb, unsigned int cnt,
                                          SetValBranch* valb);



  void gecode_svar_selector_delete(SetVarBranch* s);
  void gecode_svar_selector_delete_sap(void* s);
  void gecode_sval_selector_delete(SetValBranch* s);
  void gecode_sval_selector_delete_sap(void* s);

  /* symmetries */
  SymmetryHandle* gecode_VariableSymmetry_svars(SetVarArgs* x);
  SymmetryHandle* gecode_VariableSequenceSymmetry_svars(SetVarArgs* x, int ss);

  BrancherHandle* gecode_branch_svars_sym(CLSpace *space, SetVarArgs* vars,
                                          SetVarBranch* varb, SetValBranch* valb,
                                          Symmetries* sym);

  /* variable selectors for set branchers */
  SetVarBranch* SET_VAR_NONE(void);
  SetVarBranch* SET_VAR_RND(unsigned int seed);
  // TODO: SetVarBranch SET_VAR_MERIT_MIN(SetBranchMerit bm, BranchTbl tbl=NULL);
  // TODO: SetVarBranch SET_VAR_MERIT_MAX(SetBranchMerit bm, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_DEGREE_MIN(void);
  SetVarBranch* SET_VAR_DEGREE_MAX(void);
  SetVarBranch* SET_VAR_AFC_MIN(double d);
  // TODO: SetVarBranch SET_VAR_AFC_MIN(SetAFC a, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_AFC_MAX(double d);
  // TODO: SetVarBranch SET_VAR_AFC_MAX(SetAFC a, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_ACTIVITY_MIN(double d);
  // TODO: SetVarBranch SET_VAR_ACTIVITY_MIN(SetActivity a, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_ACTIVITY_MAX(double d);
  // TODO: SetVarBranch SET_VAR_ACTIVITY_MAX(SetActivity a, BranchTbl tbl=NULL);     
  SetVarBranch* SET_VAR_MIN_MIN(void);
  SetVarBranch* SET_VAR_MIN_MAX(void);
  SetVarBranch* SET_VAR_MAX_MIN(void);
  SetVarBranch* SET_VAR_MAX_MAX(void);
  SetVarBranch* SET_VAR_SIZE_MIN(void);
  SetVarBranch* SET_VAR_SIZE_MAX(void);
  SetVarBranch* SET_VAR_DEGREE_SIZE_MIN(void);
  SetVarBranch* SET_VAR_DEGREE_SIZE_MAX(void);
  SetVarBranch* SET_VAR_AFC_SIZE_MIN(double d);
  // TODO: SetVarBranch SET_VAR_AFC_SIZE_MIN(SetAFC a, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_AFC_SIZE_MAX(double d);
  // TODO: SetVarBranch SET_VAR_AFC_SIZE_MAX(SetAFC a, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_ACTIVITY_SIZE_MIN(double d);
  // TODO: SetVarBranch SET_VAR_ACTIVITY_SIZE_MIN(SetActivity a, BranchTbl tbl=NULL);
  SetVarBranch* SET_VAR_ACTIVITY_SIZE_MAX(double d);
  // TODO: SetVarBranch SET_VAR_ACTIVITY_SIZE_MAX(SetActivity a, BranchTbl tbl=NULL);     

  /* value selectors for set branchers */
  SetValBranch* SET_VAL_MIN_INC(void);
  SetValBranch* SET_VAL_MIN_EXC(void);
  SetValBranch* SET_VAL_MED_INC(void);
  SetValBranch* SET_VAL_MED_EXC(void);
  SetValBranch* SET_VAL_MAX_INC(void);
  SetValBranch* SET_VAL_MAX_EXC(void);
  SetValBranch* SET_VAL_RND_INC(unsigned int seed);
  SetValBranch* SET_VAL_RND_EXC(unsigned int seed);


} /* extern C */

#endif /* gecodeglue.h */
