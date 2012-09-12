extern "C" {
  /* exceptions */
  void gecode_init_exceptionHandler(void (*fptr)(const char*));

  /* callbacks */
  void gecode_init_callbackInt(void (*fptr)(unsigned, const ModEvent));

  /* space handling */
  void gecode_intClChannel(CLSpace *space, size_t x0, unsigned idx);
  CLSpace *gecode_space_create(void);
  void gecode_space_delete(CLSpace *space);
  CLSpace *gecode_space_copy(CLSpace *space);

  /* variables */
  size_t gecode_bool_addvar(CLSpace *space);
  size_t gecode_int_addvar(CLSpace *space, int min, int max);
  size_t gecode_float_addvar(CLSpace *space, double min, double max);
  STATUS gecode_get_int_info(CLSpace *space, size_t var, int *min, int *max, int *size);
  STATUS gecode_get_float_info(CLSpace *space, size_t var, double *min, double *max, double *median);

  /* branchers */
  void gecode_branch_int_var(CLSpace *space, size_t var, IntValBranch val);
  void gecode_branch_int_vars(CLSpace *space, size_t vars[], size_t count, IntVarBranch var, IntValBranch val);
  void gecode_branch_bool_var(CLSpace *space, size_t var, IntValBranch val);
  void gecode_branch_bool_vars(CLSpace *space, size_t vars[], size_t count, IntVarBranch var, IntValBranch val);

  /* search engines */
  DFS<CLSpace> *gecode_dfs_engine_create(CLSpace *space);
  void gecode_dfs_engine_delete(DFS<CLSpace> *dfs);
  BAB<CLSpace> *gecode_bab_engine_create(CLSpace *space, size_t minVar);
  void gecode_bab_engine_delete(BAB<CLSpace> *bab);
  CLSpace *gecode_bab_engine_next(BAB<CLSpace> *bab);
  

  /* ### propagator interfaces ### */

  /* FD Boolean variable relations */
  void gecode_rel_bvar_int(CLSpace *space, IntRelType op,
                           size_t v, int value, IntConLevel icl);

  void gecode_rel_bvar_bvar(CLSpace *space, IntRelType op,
                            size_t v1, size_t v2, IntConLevel icl);

  void gecode_rel_bvar_int_reified(CLSpace *space, IntRelType op, size_t v1,
                                   int val, ReifyMode mode, size_t v2, IntConLevel icl);
  
  void gecode_rel_bvar_bvar_reified(CLSpace *space, IntRelType op, size_t v1,
                                    size_t v2, ReifyMode mode, size_t b1, IntConLevel icl);

  void gecode_rel_bvars(CLSpace *space, IntRelType op, int v[],
                        size_t count, IntConLevel icl);

  void gecode_rel_bvars_int(CLSpace *space, IntRelType op, int v[],
                            size_t count, int value, IntConLevel icl);

  void gecode_rel_bvars_bvars(CLSpace *space, IntRelType op,
                              int v1[], size_t count1, int v2[], size_t count2,
                              IntConLevel icl);


  /* Boolean operations */
  void gecode_op_bvar_bvar_bvar(CLSpace *space, BoolOpType op, size_t v1,
                                size_t v2, size_t b1, IntConLevel icl);

  void gecode_op_bvar_bvar_int(CLSpace *space, BoolOpType op, size_t v1,
                               size_t v2, int n, IntConLevel icl);

  void gecode_op_bvars_int(CLSpace *space, BoolOpType op, int v[],
                           size_t count, int value, IntConLevel icl);

  void gecode_op_bvars_bvar(CLSpace *space, BoolOpType op, int v[],
                            size_t count, int var, IntConLevel icl);

  void gecode_clause_bvars_bvars_int(CLSpace *space, BoolOpType op,
                                     int v1[], size_t count1,
                                     int v2[], size_t count2,
                                     int n, IntConLevel icl);

  void gecode_clause_bvars_bvars_bvar(CLSpace *space, BoolOpType op,
                                      int v1[], size_t count1,
                                      int v2[], size_t count2,
                                      int bvar, IntConLevel icl);


  /* integer relations */
  void gecode_rel_ivar_int(CLSpace *space, IntRelType op,
                           size_t v, int value, IntConLevel icl);

  void gecode_rel_ivar_ivar(CLSpace *space, IntRelType op,
                            size_t v1, size_t v2, IntConLevel icl);

  void gecode_rel_ivar_int_reified(CLSpace *space, IntRelType op, size_t v1,
                                   int val, ReifyMode mode, size_t b1, IntConLevel icl);

  void gecode_rel_ivar_ivar_reified(CLSpace *space, IntRelType op, size_t v1,
                                    size_t v2, ReifyMode mode, size_t b1, IntConLevel icl);

  void gecode_rel_ivars(CLSpace *space, IntRelType op, int v[],
                        size_t count, IntConLevel icl);

  void gecode_rel_ivars_int(CLSpace *space, IntRelType op, int v[],
                            size_t count, int value, IntConLevel icl);

  void gecode_rel_ivars_ivars(CLSpace *space, IntRelType op,
                              int v1[], size_t count1,
                              int v2[], size_t count2,
                              IntConLevel icl);


  /* distinct constraint */
  void gecode_distinct_ivars(CLSpace *space, const  int vars[], 
                             size_t count, IntConLevel icl);

  void gecode_distinct_ints_ivars(CLSpace *space, const int *intoffsets, const int *vars, 
                                  size_t count, IntConLevel icl);


  /* sorted constraint */
  void gecode_sorted_ivars_ivars(CLSpace *space, const  int xvars[], const  int yvars[], 
                                 size_t count, IntConLevel icl);

  void gecode_sorted_ivars_ivars_ivars(CLSpace *space, const  int xvars[],
                                       const  int yvars[], const  int zvars[], 
                                       size_t count, IntConLevel icl);


  /* channeling constraints */
  void gecode_channel_ivars_ivars(CLSpace *space, int v1[], int v2[],
                                  size_t count, IntConLevel icl);

  void gecode_channel_ivars_int_ivars_int(CLSpace *space,
                                          int v1[], int xoff,
                                          int v2[], int yoff,
                                          size_t count, IntConLevel icl);

  void gecode_channel_bvar_ivar(CLSpace *space,
                                size_t bvar, size_t ivar,
                                IntConLevel icl);

  void gecode_channel_ivar_bvar(CLSpace *space,
                                size_t ivar, size_t bvar,
                                IntConLevel icl);

  void gecode_channel_bvars_ivar_int(CLSpace *space,
                                     int v1[], size_t count,
                                     size_t ivar, int offset,
                                     IntConLevel icl);


  /* integer arithmetic constraints */
  void gecode_min_ivar_ivar_ivar(CLSpace *space,
                                 size_t x1, size_t x2,
                                 size_t ret, IntConLevel icl);

  void gecode_min_ivars_ivar(CLSpace *space,
                             int v[], size_t count, int y,
                             IntConLevel icl);

  void gecode_max_ivar_ivar_ivar(CLSpace *space,
                                 size_t x1, size_t x2,
                                 size_t ret, IntConLevel icl);

  void gecode_max_ivars_ivar(CLSpace *space,
                             int v[], size_t count,
                             int y, IntConLevel icl);

  void gecode_abs_ivar_ivar(CLSpace *space,
                            size_t x1, size_t x2,
                            IntConLevel icl);

  void gecode_mul_ivar_ivar_ivar(CLSpace *space,
                                 size_t x1, size_t x2,
                                 size_t ret, IntConLevel icl);

  void gecode_sqr_ivar_ivar(CLSpace *space,
                            size_t x1, size_t x2,
                            IntConLevel icl);

  void gecode_sqrt_ivar_ivar(CLSpace *space,
                             size_t x1, size_t x2,
                             IntConLevel icl);

  void gecode_divmod_ivar_ivar_ivar_ivar(CLSpace *space,
                                         size_t x0, size_t x1,
                                         size_t x2, size_t x3,
                                         IntConLevel icl);

  void gecode_div_ivar_ivar_ivar(CLSpace *space,
                                 size_t x1, size_t x2,
                                 size_t ret, IntConLevel icl);

  void gecode_mod_ivar_ivar_ivar(CLSpace *space,
                                 size_t x1, size_t x2,
                                 size_t ret, IntConLevel icl);


  /* integer linear constraint */
  void gecode_lin_ivars_int(CLSpace *space, IntRelType rel, int v[],
                            size_t count, int value, IntConLevel icl);

  void gecode_lin_ivars_ivar(CLSpace *space, IntRelType rel, int v[],
                             size_t count, int var, IntConLevel icl);

  void gecode_lin_ivars_int_reified(CLSpace *space, IntRelType rel, int v[],
                                    size_t count, int value, ReifyMode mode,
                                    int bvar, IntConLevel icl);

  void gecode_lin_ivars_ivar_reified(CLSpace *space, IntRelType rel, int v[],
                                     size_t count, int var, ReifyMode mode, 
                                     int bvar, IntConLevel icl);

  void gecode_lin_ints_ivars_int(CLSpace *space, IntRelType rel, int ints[], int v[],
                                 size_t count, int value, IntConLevel icl);

  void gecode_lin_ints_ivars_ivar(CLSpace *space, IntRelType rel, int ints[], int v[],
                                  size_t count, int var, IntConLevel icl);

  void gecode_lin_ints_ivars_int_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                         size_t count, int value, ReifyMode mode, int bvar, IntConLevel icl);

  void gecode_lin_ints_ivars_ivar_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                          size_t count, int var, ReifyMode mode, int bvar, IntConLevel icl);


  /* boolean linear constraint */
  void gecode_lin_bvars_int(CLSpace *space, IntRelType rel, int v[],
                            size_t count, int value, IntConLevel icl);

  void gecode_lin_bvars_ivar(CLSpace *space, IntRelType rel, int v[],
                             size_t count, int var, IntConLevel icl);

  void gecode_lin_bvars_int_reified(CLSpace *space, IntRelType rel, int v[],
                                    size_t count, int value, ReifyMode mode, int bvar, IntConLevel icl);

  void gecode_lin_bvars_ivar_reified(CLSpace *space, IntRelType rel, int v[],
                                     size_t count, int var, ReifyMode mode, int bvar, IntConLevel icl);

  void gecode_lin_ints_bvars_int(CLSpace *space, IntRelType rel, int ints[], int v[],
                                 size_t count, int value, IntConLevel icl);

  void gecode_lin_ints_bvars_ivar(CLSpace *space, IntRelType rel, int ints[], int v[],
                                  size_t count, int var, IntConLevel icl);

  void gecode_lin_ints_bvars_int_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                         size_t count, int value, ReifyMode mode, 
                                         int bvar, IntConLevel icl);

  void gecode_lin_ints_bvars_ivar_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                          size_t count, int var, ReifyMode mode, 
                                          int bvar, IntConLevel icl);


} /* extern C */
