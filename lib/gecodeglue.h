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

  /* integer argument arrays */
  CLIntArgs* gecode_intargs_create(int n);
  int *gecode_intargs_adr(CLIntArgs *v);
  void gecode_intargs_delete(CLIntArgs *v);

  /* double argument arrays */
  CLFloatArgs* gecode_floatargs_create(int n);
  void gecode_floatargs_set(CLFloatArgs *v, int i, double e);
  void gecode_floatargs_delete(CLFloatArgs *v);


  /* exceptions */
  void gecode_init_exceptionHandler(void (*fptr)(const char*));

  /* callbacks */
  void gecode_init_callbackInt(void (*fptr)(unsigned, const ModEvent));

  /* space handling */
  void gecode_intClChannel(CLSpace *space, size_t x0, unsigned idx);
  CLSpace* gecode_space_create(void);
  void gecode_space_delete(CLSpace *space);
  CLSpace* gecode_space_copy(CLSpace *space);

  /* variables */
  size_t gecode_bool_addvar(CLSpace *space);
  size_t gecode_int_addvar(CLSpace *space, int min, int max);
  size_t gecode_int_addvar_set(CLSpace *space, IntSet* set);
  size_t gecode_float_addvar(CLSpace *space, double min, double max);
  BoolVar* gecode_get_boolvar_by_index(CLSpace *space, size_t index);
  IntVar* gecode_get_intvar_by_index(CLSpace *space, size_t index);
  FloatVar* gecode_get_floatvar_by_index(CLSpace *space, size_t index);
  STATUS gecode_get_bool_info(CLSpace *space, BoolVar* var, int *value);
  STATUS gecode_get_int_info(CLSpace *space, IntVar* var, 
			     int *min, int *max, int *size);
  STATUS gecode_get_float_info(CLSpace *space, FloatVar* var,
			       double *min, double *max, double *median);

  /* branchers */
  void gecode_branch_ivar(CLSpace *space, IntVar* var, IntValBranch* valb);
  void gecode_branch_ivars(CLSpace *space, IntVarArgs* vars,
                           IntVarBranch* varb, IntValBranch* valb);

  void gecode_branch_bvar(CLSpace *space, BoolVar* var, IntValBranch* valb);
  void gecode_branch_bvars(CLSpace *space, BoolVarArgs* vars,
                           IntVarBranch* varb, IntValBranch* valb);

  /* variable selectors for branchers */
  void gecode_ivar_selector_delete(IntVarBranch* s);
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
  IntVarBranch* INT_VAR_SIZE_DEGREE_MIN(void);
  IntVarBranch* INT_VAR_SIZE_DEGREE_MAX(void);
  IntVarBranch* INT_VAR_SIZE_AFC_MIN(void);
  IntVarBranch* INT_VAR_SIZE_AFC_MAX(void);
  /* TODO: check whether to insert ACTIVITY variants */
  IntVarBranch* INT_VAR_REGRET_MIN_MIN(void);
  IntVarBranch* INT_VAR_REGRET_MIN_MAX(void);
  IntVarBranch* INT_VAR_REGRET_MAX_MIN(void);
  IntVarBranch* INT_VAR_REGRET_MAX_MAX(void);

  /* value selectors for branchers */
  void gecode_ival_selector_delete(IntValBranch* s);
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

  /* search engines */
  DFS<CLSpace> *gecode_dfs_engine_create(CLSpace *space);
  void gecode_dfs_engine_delete(DFS<CLSpace> *dfs);
  CLSpace *gecode_dfs_engine_next(DFS<CLSpace> *dfs);
  BAB<CLSpace> *gecode_bab_engine_create(CLSpace *space, size_t minVar);
  void gecode_bab_engine_delete(BAB<CLSpace> *bab);
  CLSpace *gecode_bab_engine_next(BAB<CLSpace> *bab);
  

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
  void gecode_pow_fvar_uint_fvar(CLSpace *space, FloatVar* x0, unsigned int pow, FloatVar* x1);
  void gecode_nroot_fvar_uint_fvar(CLSpace *space, FloatVar* x0, unsigned int pow, FloatVar* x1);
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
  void gecode_lin_fargs_fvars_dbl(CLSpace *space, FloatRelType rel, FloatArgs* a, FloatVarArgs* x,
				  double c);
  void gecode_lin_fargs_fvars_fvar(CLSpace *space, FloatRelType rel, FloatArgs* a, FloatVarArgs* x,
				   FloatVar* y);
  void gecode_lin_fargs_fvars_dbl_reified(CLSpace *space, FloatRelType rel, FloatArgs* a,
					  FloatVarArgs* x, double c,
					  ReifyMode mode, BoolVar* bvar);
  void gecode_lin_fargs_fvars_fvar_reified(CLSpace *space, FloatRelType rel, FloatArgs* a,
					   FloatVarArgs* x, FloatVar* y,
					   ReifyMode mode, BoolVar* bvar);

  void gecode_channel_fvar_ivar(CLSpace *space, FloatVar* x0, IntVar* x1);

  // Float Branchers
  void gecode_branch_fvar(CLSpace *space, FloatVar* var, FloatValBranch* valb);
  void gecode_branch_fvars(CLSpace *space, FloatVarArgs* vars,
			   FloatVarBranch* varb, FloatValBranch* valb);

  void gecode_fvar_selector_delete(FloatVarBranch* s);

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
  FloatVarBranch* FLOAT_VAR_SIZE_DEGREE_MIN(void);
  FloatVarBranch* FLOAT_VAR_SIZE_DEGREE_MAX(void);
  FloatVarBranch* FLOAT_VAR_SIZE_AFC_MIN(void);
  FloatVarBranch* FLOAT_VAR_SIZE_AFC_MAX(void);
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

} /* extern C */

#endif /* gecodeglue.h */
