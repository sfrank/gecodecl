#include <vector>
#include <exception>

#include "gecode/kernel.hh"
#include "gecode/support.hh"
#include "gecode/int.hh"
#include "gecode/float.hh"
#include "gecode/set.hh"
#include "gecode/search.hh"
#include "gecode/minimodel.hh"

#define EXCSTART try {
#define EXCSTOP  }                                 \
                 catch (Exception &e) {            \
                   exceptionHandlerCL(e.what());   \
                 }                                 \
                 catch (exception &e) {            \
                   exceptionHandlerCL(e.what());   \
                 }

using namespace std;
using namespace Gecode;

extern "C" {
void (*exceptionHandlerCL)(const char*) = NULL;
void gecode_init_exceptionHandler(void (*fptr)(const char*)) {
  exceptionHandlerCL = fptr;
}

void (*callbackFunInt)(unsigned, const ModEvent) = NULL;
void gecode_init_callbackInt(void (*fptr)(unsigned, const ModEvent)) {
  callbackFunInt = fptr;
}
} /* extern "C" */

class CLintnotifier : public Propagator {
protected:
  Council<ViewAdvisor<Int::IntView> > c;
  unsigned callbackidx;

public:
  CLintnotifier(Home home, const IntVar& x, const unsigned idx) 
    : Propagator(home), c(home), callbackidx(idx) {
    (void) new (home) ViewAdvisor<Int::IntView>(home,*this,c,x);
  }
  static ExecStatus post(Home home, 
                         const IntVar& x, const unsigned idx) {
    (void) new (home) CLintnotifier(home,x, idx);
    return ES_OK;
  }
  CLintnotifier(Space& home, bool share, CLintnotifier& p) 
    : Propagator(home,share,p) {
    callbackidx = p.callbackidx;
    c.update(home,share,p.c);
  }
  virtual Propagator* copy(Space& home, bool share) {
    return new (home) CLintnotifier(home,share,*this);
  }
  virtual PropCost cost(const Space&, const ModEventDelta&) const {
    // cost irrelevant, callback propagator will never be scheduled, 
    // only the advise method will be called
    return PropCost::unary(PropCost::HI); 
  }
  virtual ExecStatus advise(Space&, Advisor& a, const Delta& d) 
  {
    callbackFunInt(callbackidx,
                   static_cast<ViewAdvisor<Int::IntView>&>(a).view().modevent(d));
    return ES_FIX;
  }
  
  virtual ExecStatus propagate(Space&, const ModEventDelta&) {
    return ES_FIX;
  }
};

enum STATUS {STATE_FAILED = -2, VAR_UNASSIGNED = -1, VAR_ASSIGNED = 0};

class CLSpace : public Space {
private:
  vector<BoolVar> boolVariables;
  vector<IntVar>  intVariables;
  vector<FloatVar>  floatVariables;
  vector<SetVar>  setVariables;

  vector<IntVar>::size_type minimizeVar;
public:

  CLSpace() : boolVariables(), intVariables(), floatVariables(), setVariables() {
    minimizeVar = (vector<IntVar>::size_type)UINT_MAX;
  }

  // Constructor for cloning a space
  CLSpace(bool share, CLSpace &space) : Space(share,space),
                                        boolVariables(space.boolVariables.size()),
                                        intVariables(space.intVariables.size()),
                                        floatVariables(space.floatVariables.size()),
                                        setVariables(space.setVariables.size())
  {
    minimizeVar = space.minimizeVar;

    unsigned size;
    size = space.boolVariables.size();
    for (unsigned i=0; i<size; i++) {
      boolVariables.at(i).update(*this, share, space.boolVariables.at(i));
    }
    size = space.intVariables.size();
    for (unsigned i=0; i<size; i++) {
      intVariables.at(i).update(*this, share, space.intVariables.at(i));
    }
    size = space.floatVariables.size();
    for (unsigned i=0; i<size; i++) {
      floatVariables.at(i).update(*this, share, space.floatVariables.at(i));
    }
    size = space.setVariables.size();
    for (unsigned i=0; i<size; i++) {
      setVariables.at(i).update(*this, share, space.setVariables.at(i));
    }
  }

  virtual Space *copy(bool share) {
    return new CLSpace(share, *this);
  }

  void inline constrain(const Space& _best) {
    const CLSpace& best = static_cast<const CLSpace&>(_best);
    rel(*this, cost(), IRT_LE, best.cost().val());
  }

  IntVar inline cost() const {
    return intVariables[minimizeVar];
  }

  void inline setCost(vector<IntVar>::size_type minVar) {
    minimizeVar = minVar;
  }

  ~CLSpace() {}

  // Boolean variables

  vector<BoolVar>::size_type addBoolVariable(void) {
    BoolVar var(*this,0,1);
    boolVariables.push_back(var);
    return boolVariables.size() - 1;
  }

  BoolVar inline getBoolVar(vector<BoolVar>::size_type var) const {
    assert(var < boolVariables.size());
    return (boolVariables[var]);
  }

  vector<BoolVar>::size_type inline getBoolVarSize(void) {
    return boolVariables.size();
  }

  STATUS inline getBoolInfo(vector<BoolVar>::size_type var, int *value) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      return STATE_FAILED;
    }

    BoolVar v = getBoolVar(var);
    if (v.none()) {
      return VAR_UNASSIGNED;
    } else {
      *value = v.val();
      return VAR_ASSIGNED;
    }
  }

  // integer variables

  vector<IntVar>::size_type addIntVariable(int min, int max) {
    IntVar var(*this, min, max);
    intVariables.push_back(var);
    return intVariables.size() - 1;
  }

  IntVar getIntVar(vector<IntVar>::size_type var) const {
    assert(var < intVariables.size());
    return (intVariables[var]);
  }

  IntVar *getIntVarp(vector<IntVar>::size_type var) {
    return &(intVariables[var]);
  }

  vector<IntVar>::size_type inline getIntVarSize(void) {
    return intVariables.size();
  }

  /*   
  STATUS inline getIntInfo(vector<IntVar>::size_type var,
                           int *min, int *max, int *size) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      *size = 0;
      *min = 0;
      *max = 0;
      return STATE_FAILED;
    }

    IntVar v = getIntVar(var);
    *size = v.size();
    if (*size > 1) {
      *min = v.min();
      *max = v.max();
      return VAR_UNASSIGNED;
    } else {
      *min = v.val();
      *max = *min;
      return VAR_ASSIGNED;
    }
  }
  */
  STATUS inline getIntInfo(IntVar *var,
                           int *min, int *max, int *size) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      *size = 0;
      *min = 0;
      *max = 0;
      return STATE_FAILED;
    }

    *size = var->size();
    if (*size > 1) {
      *min = var->min();
      *max = var->max();
      return VAR_UNASSIGNED;
    } else {
      *min = var->val();
      *max = *min;
      return VAR_ASSIGNED;
    }
  }

  // float variables

  vector<FloatVar>::size_type addFloatVariable(float min, float max) {
    FloatVar var(*this, min, max);
    floatVariables.push_back(var);
    return floatVariables.size() - 1;
  }

  FloatVar inline getFloatVar(vector<FloatVar>::size_type var) const {
    assert(var < floatVariables.size());
    return (floatVariables[var]);
  }

  vector<FloatVar>::size_type inline getFloatVarSize(void) {
    return floatVariables.size();
  }
 
  STATUS inline getFloatInfo(vector<FloatVar>::size_type var,
                           double *min, double *max, double *med) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      *med = 0.0;
      *min = 0.0;
      *max = 0.0;
      return STATE_FAILED;
    }

    FloatVar v = getFloatVar(var);
    *min = v.min();
    *max = v.max();
    *med = v.med();
    if (v.size() == 0.0) {
      return VAR_ASSIGNED;
    }
    else {
      return VAR_UNASSIGNED;
    }
  }

  /*
  int addSetVariable(int low, int high) {
    SetVar var(*this,low,high);
    setVariables.push_back(var);
    return setVariables.size() - 1;
  }
  */
};

class CLVarArgs : public VarArgArray<IntVar> {
public:
  static void* operator new(size_t) {
    return ::operator new(sizeof(CLVarArgs));
  }

  static void  operator delete(void* v) {
    ::operator delete(v);
  };
  
  CLVarArgs(int n) : VarArgArray<IntVar>(n){};
  ~CLVarArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  void set(int i, const IntVar* e) {
    a[i] = *e;
  }
};

class CLIntArgs : public IntArgs {
public:
  static void* operator new(size_t) {
    return ::operator new(sizeof(CLIntArgs));
  }

  static void  operator delete(void* v) {
    ::operator delete(v);
  };
  
  CLIntArgs(int n) : IntArgs(n){};
  ~CLIntArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  void set(int i, int e) {
    a[i] = e;
  }

  int *adr(void) {
    return a;
  }
};

#include "gecodeglue.h"

extern "C" {
  
CLVarArgs *gecode_varargs_create(int n) {
    return new CLVarArgs(n); 
}
void gecode_varargs_set(CLVarArgs *v, int i, const IntVar* e) {
  v->set(i, e);
}
void gecode_varargs_delete(CLVarArgs *v) {
  delete v; }

CLIntArgs *gecode_intargs_create(int n) {
    return new CLIntArgs(n); 
}
void gecode_intargs_set(CLIntArgs *v, int i, int e) {
  v->set(i, e);
}
int *gecode_intargs_adr(CLIntArgs *v) {
  return v->adr();
}
void gecode_intargs_delete(CLIntArgs *v) {
  delete v; }


void gecode_intClChannel(CLSpace *space, vector<IntVar>::size_type x0, unsigned idx) {
  Int::IntView x(space->getIntVar(x0));
  if (space->failed()) {
    return;
  }
  else if(CLintnotifier::post(*space,x,idx) != ES_OK) 
    { space->fail(); }
}

CLSpace *gecode_space_create(void) { return new CLSpace(); }

void gecode_space_delete(CLSpace *space) { delete space; }

CLSpace *gecode_space_copy(CLSpace *space) {
  return (CLSpace *) space->clone(false); }

SpaceStatus gecode_space_status(CLSpace *space){
  return space->status(); }

size_t gecode_bool_addvar(CLSpace *space) {
  return space->addBoolVariable(); }

size_t gecode_int_addvar(CLSpace *space, int min, int max) {
  return space->addIntVariable(min, max); }

size_t gecode_float_addvar(CLSpace *space, double min, double max) {
  return space->addFloatVariable(min, max); }


BoolVar gecode_get_boolvar_by_index(CLSpace *space, size_t index) {
  return space->getBoolVar(index);
}

IntVar *gecode_get_intvar_by_index(CLSpace *space, size_t index) {
  return space->getIntVarp(index);
}

FloatVar gecode_get_floatvar_by_index(CLSpace *space, size_t index) {
  return space->getFloatVar(index);
}


STATUS gecode_get_bool_info(CLSpace *space, size_t var, int *value) {
  return space->getBoolInfo(var, value); }

STATUS gecode_get_int_info(CLSpace *space, IntVar *var,
                           int *min, int *max, int *size) {
  return space->getIntInfo(var, min, max, size); }

STATUS gecode_get_float_info(CLSpace *space, size_t var,
                           double *min, double *max, double *median) {
  return space->getFloatInfo(var, min, max, median); }


// Branchers
IntVarBranch INT_VAR_NONE(void){
  return Gecode::INT_VAR_NONE();
}
IntVarBranch INT_VAR_RND(unsigned int seed){
  return Gecode::INT_VAR_RND(Rnd(seed));
}
/* TODO: check whether to insert MERIT variants */
IntVarBranch INT_VAR_DEGREE_MIN(void){
  return Gecode::INT_VAR_DEGREE_MIN(NULL);
}
IntVarBranch INT_VAR_DEGREE_MAX(void){
  return Gecode::INT_VAR_DEGREE_MAX(NULL);
}
IntVarBranch INT_VAR_AFC_MIN(void){
  return Gecode::INT_VAR_AFC_MIN(NULL);
}
IntVarBranch INT_VAR_AFC_MAX(void){
  return Gecode::INT_VAR_AFC_MAX(NULL);
}
/* TODO: check whether to insert ACTIVITY variants */
/*
IntVarBranch INT_VAR_ACTIVITY_MIN_ints
  (CLSpace *space, size_t *vars, size_t count, double d)
{
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(vars[i]); }
  return INT_VAR_ACTIVITY_MIN(IntActivity(*space,va,d));
}
*/
IntVarBranch INT_VAR_MIN_MIN(void){
  return Gecode::INT_VAR_MIN_MIN(NULL);
}
IntVarBranch INT_VAR_MIN_MAX(void){
  return Gecode::INT_VAR_MIN_MAX(NULL);
}
IntVarBranch INT_VAR_MAX_MIN(void){
  return Gecode::INT_VAR_MAX_MIN(NULL);
}
IntVarBranch INT_VAR_MAX_MAX(void){
  return Gecode::INT_VAR_MAX_MAX(NULL);
}
IntVarBranch INT_VAR_SIZE_MIN(void){
  return Gecode::INT_VAR_SIZE_MIN(NULL);
}
IntVarBranch INT_VAR_SIZE_MAX(void){
  return Gecode::INT_VAR_SIZE_MAX(NULL);
}
IntVarBranch INT_VAR_SIZE_DEGREE_MIN(void){
  return Gecode::INT_VAR_SIZE_DEGREE_MIN(NULL);
}
IntVarBranch INT_VAR_SIZE_DEGREE_MAX(void){
  return Gecode::INT_VAR_SIZE_DEGREE_MAX(NULL);
}
IntVarBranch INT_VAR_SIZE_AFC_MIN(void){
  return Gecode::INT_VAR_SIZE_AFC_MIN(NULL);
}
IntVarBranch INT_VAR_SIZE_AFC_MAX(void){
  return Gecode::INT_VAR_SIZE_AFC_MAX(NULL);
}
/* TODO: check whether to insert ACTIVITY variants */
IntVarBranch INT_VAR_REGRET_MIN_MIN(void){
  return Gecode::INT_VAR_REGRET_MIN_MIN(NULL);
}
IntVarBranch INT_VAR_REGRET_MIN_MAX(void){
  return Gecode::INT_VAR_REGRET_MIN_MAX(NULL);
}
IntVarBranch INT_VAR_REGRET_MAX_MIN(void){
  return Gecode::INT_VAR_REGRET_MAX_MIN(NULL);
}
IntVarBranch INT_VAR_REGRET_MAX_MAX(void){
  return Gecode::INT_VAR_REGRET_MAX_MAX(NULL);
}

IntValBranch INT_VAL_MIN(void){
  return Gecode::INT_VAL_MIN();
}
IntValBranch INT_VAL_MED(void){
  return Gecode::INT_VAL_MED();
}
IntValBranch INT_VAL_MAX(void){
  return Gecode::INT_VAL_MAX();
}
IntValBranch INT_VAL_RND(unsigned int seed){
  return Gecode::INT_VAL_RND(Rnd(seed));
}
IntValBranch INT_VAL_SPLIT_MIN(void){
  return Gecode::INT_VAL_SPLIT_MIN();
}
IntValBranch INT_VAL_SPLIT_MAX(void){
  return Gecode::INT_VAL_SPLIT_MAX();
}
IntValBranch INT_VAL_RANGE_MIN(void){
  return Gecode::INT_VAL_RANGE_MIN();
}
IntValBranch INT_VAL_RANGE_MAX(void){
  return Gecode::INT_VAL_RANGE_MAX();
}
IntValBranch INT_VALUES_MIN(void){
  return Gecode::INT_VALUES_MIN();
}
IntValBranch INT_VALUES_MAX(void){
  return Gecode::INT_VALUES_MAX();
}

void gecode_branch_int_var(CLSpace *space, size_t var, IntValBranch val) {
  branch(*space, space->getIntVar(var), val);
}

void gecode_branch_int_vars(CLSpace *space, size_t *vars, vector<IntVar>::size_type count,
                            IntVarBranch var, IntValBranch val) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(vars[i]); }
  branch(*space, va, var, val);
}

void gecode_branch_bool_var(CLSpace *space, size_t var, IntValBranch val) {
  branch(*space, space->getBoolVar(var), val);
}

void gecode_branch_bool_vars(CLSpace *space, size_t *vars, vector<BoolVar>::size_type count,
                            IntVarBranch var, IntValBranch val) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(vars[i]); }
  branch(*space, va, var, val);
}


// Search engines

void inline checkBranchers(CLSpace *space) {
  if(space->branchers() == 0) {
    int size = 0;
    
    // branch on all IntVars
    size = space->getIntVarSize();
    if(size == 1) {
      branch(*space,space->getIntVar(0), Gecode::INT_VAL_SPLIT_MIN());
    }
    else if(size > 1) {
      IntVarArgs vars(size);
      for(int i=0; i<size; i++) {
        vars[i] = space->getIntVar(i);
      }
      branch(*space, vars, Gecode::INT_VAR_SIZE_MIN(), Gecode::INT_VAL_SPLIT_MIN());
    }
    
    // branch on all BoolVars
    size = space->getBoolVarSize();
    if(size == 1) {
      branch(*space,space->getBoolVar(0), Gecode::INT_VAL_MIN());
    }
    else if(size > 1) {
      BoolVarArgs vars(size);
      for(int i=0; i<size; i++) {
        vars[i] = space->getBoolVar(i);
      }
      branch(*space, vars, Gecode::INT_VAR_AFC_MIN(), Gecode::INT_VAL_MIN());
    }
  }
}

DFS<CLSpace> *gecode_dfs_engine_create(CLSpace *space){
  EXCSTART
    Search::Options options = Search::Options();
    
    // check whether there are any branchers defined at all
    checkBranchers(space);
    return (new DFS<CLSpace>(space, options));
  EXCSTOP
  return NULL;
}

void gecode_dfs_engine_delete(DFS<CLSpace> *dfs){ delete dfs;}

CLSpace *gecode_dfs_engine_next(DFS<CLSpace> *dfs){
  return dfs->next();
}

BAB<CLSpace> *gecode_bab_engine_create(CLSpace *space,
                                       vector<IntVar>::size_type minVar){
  EXCSTART
    space->setCost(minVar);
    Search::Options options = Search::Options();
    // check whether there are any branchers defined at all
    checkBranchers(space);
    return new BAB<CLSpace>(space, options); 
  EXCSTOP
  return NULL;
}

void gecode_bab_engine_delete(BAB<CLSpace> *bab){ delete bab;}

CLSpace *gecode_bab_engine_next(BAB<CLSpace> *bab){
  EXCSTART
    return bab->next();
  EXCSTOP
  return NULL;
}


// Propagator interfaces 

/* simple relation constraints */

/* finite domain boolean relations */

void gecode_rel_bvar_int(CLSpace *space, IntRelType op,
                         size_t v, int value, IntConLevel icl) {
  rel(*space, space->getBoolVar(v), op, value, icl); }

void gecode_rel_bvar_bvar(CLSpace *space, IntRelType op,
                          size_t v1, size_t v2, IntConLevel icl) {
  rel(*space, space->getBoolVar(v1), op, space->getBoolVar(v2), icl); }

void gecode_rel_bvar_int_reified(CLSpace *space, IntRelType op, size_t v1,
                                 int val, ReifyMode mode, size_t v2, IntConLevel icl) {
  rel(*space, space->getBoolVar(v1), op, val, Reify(space->getBoolVar(v2), mode), icl); }

void gecode_rel_bvar_bvar_reified(CLSpace *space, IntRelType op, size_t v1,
                                  size_t v2, ReifyMode mode, size_t b1, IntConLevel icl) {
  rel(*space, space->getBoolVar(v1), op, space->getBoolVar(v2),
      Reify(space->getBoolVar(b1), mode), icl);
}

void gecode_rel_bvars(CLSpace *space, IntRelType op, int v[],
                      vector<IntVar>::size_type count, IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  rel(*space, va, op, icl);
}

void gecode_rel_bvars_int(CLSpace *space, IntRelType op, int v[],
                          vector<IntVar>::size_type count, int value,
                          IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  rel(*space, va, op, value, icl);
}

void gecode_rel_bvars_bvars(CLSpace *space, IntRelType op,
                            int v1[], vector<IntVar>::size_type count1,
                            int v2[], vector<IntVar>::size_type count2,
                            IntConLevel icl) {
  BoolVarArgs v1a(count1);
  BoolVarArgs v2a(count2);
  for(unsigned i=0; i<count1; i++)
    { v1a[i]=space->getBoolVar(v1[i]); }
  for(unsigned i=0; i<count2; i++)
    { v2a[i]=space->getBoolVar(v2[i]); }
  rel(*space, v1a, op, v2a, icl);
}


/* boolean operations */

void gecode_op_bvar_bvar_bvar(CLSpace *space, BoolOpType op, size_t v1,
                              size_t v2, size_t b1, IntConLevel icl) {
  rel(*space, space->getBoolVar(v1), op, space->getBoolVar(v2),
      space->getBoolVar(b1), icl);
}

void gecode_op_bvar_bvar_int(CLSpace *space, BoolOpType op, size_t v1,
                             size_t v2, int n, IntConLevel icl) {
  rel(*space, space->getBoolVar(v1), op, space->getBoolVar(v2), n, icl);
}

void gecode_op_bvars_int(CLSpace *space, BoolOpType op, int v[],
                          vector<BoolVar>::size_type count, int value,
                          IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  rel(*space, op, va, value, icl);
}

void gecode_op_bvars_bvar(CLSpace *space, BoolOpType op, int v[],
                          vector<BoolVar>::size_type count, int var,
                          IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  rel(*space, op, va, space->getBoolVar(var), icl);
}

void gecode_clause_bvars_bvars_int(CLSpace *space, BoolOpType op,
                                   int v1[], vector<BoolVar>::size_type count1,
                                   int v2[], vector<BoolVar>::size_type count2,
                                   int n,
                                   IntConLevel icl) {
  BoolVarArgs v1a(count1);
  BoolVarArgs v2a(count2);
  for(unsigned i=0; i<count1; i++)
    { v1a[i]=space->getBoolVar(v1[i]); }
  for(unsigned i=0; i<count2; i++)
    { v2a[i]=space->getBoolVar(v2[i]); }
  clause(*space, op, v1a, v2a, n, icl);
}

void gecode_clause_bvars_bvars_bvar(CLSpace *space, BoolOpType op,
                                    int v1[], vector<BoolVar>::size_type count1,
                                    int v2[], vector<BoolVar>::size_type count2,
                                    int bvar,
                                    IntConLevel icl) {
  BoolVarArgs v1a(count1);
  BoolVarArgs v2a(count2);
  for(unsigned i=0; i<count1; i++)
    { v1a[i]=space->getBoolVar(v1[i]); }
  for(unsigned i=0; i<count2; i++)
    { v2a[i]=space->getBoolVar(v2[i]); }
  clause(*space, op, v1a, v2a, space->getBoolVar(bvar), icl);
}

/* finite domain integer relations */

void gecode_rel_ivar_int(CLSpace *space, IntRelType op,
                         size_t v, int value, IntConLevel icl) {
  rel(*space, space->getIntVar(v), op, value, icl); }

void gecode_rel_ivar_ivar(CLSpace *space, IntRelType op,
                          size_t v1, size_t v2, IntConLevel icl) {
  rel(*space, space->getIntVar(v1), op, space->getIntVar(v2), icl); }

void gecode_rel_ivar_int_reified(CLSpace *space, IntRelType op, size_t v1,
                                 int val, ReifyMode mode, size_t b1, IntConLevel icl) {
  rel(*space, space->getIntVar(v1), op, val, 
      Reify(space->getBoolVar(b1), mode), icl); }

void gecode_rel_ivar_ivar_reified(CLSpace *space, IntRelType op, size_t v1,
                                  size_t v2, ReifyMode mode, size_t b1, IntConLevel icl) {
  rel(*space, space->getIntVar(v1), op, space->getIntVar(v2),
      Reify(space->getBoolVar(b1), mode), icl);
}

void gecode_rel_ivars(CLSpace *space, IntRelType op, int v[],
                      vector<IntVar>::size_type count, IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  rel(*space, va, op, icl);
}


void gecode_rel_ivars_int(CLSpace *space, IntRelType op, int v[],
                          vector<IntVar>::size_type count, int value,
                          IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  rel(*space, va, op, value, icl);
}

void gecode_rel_ivars_ivars(CLSpace *space, IntRelType op,
                            int v1[], vector<IntVar>::size_type count1,
                            int v2[], vector<IntVar>::size_type count2,
                            IntConLevel icl) {
  IntVarArgs v1a(count1);
  IntVarArgs v2a(count2);
  for(unsigned i=0; i<count1; i++)
    { v1a[i]=space->getIntVar(v1[i]); }
  for(unsigned i=0; i<count2; i++)
    { v2a[i]=space->getIntVar(v2[i]); }
  rel(*space, v1a, op, v2a, icl);
}


/* distinct constraint */
void gecode_dst_ivars(CLSpace *space, IntVarArgs *va, IntConLevel icl) {
  EXCSTART
    distinct(*space, *va, icl);
  EXCSTOP
}

void gecode_dst_ints_ivars(CLSpace *space, IntArgs *ia, IntVarArgs *va, IntConLevel icl) {
  EXCSTART
    distinct(*space, *ia, *va, icl);
  EXCSTOP
}


void gecode_distinct_ivars(CLSpace *space, const  int vars[], 
                           vector<IntVar>::size_type count,
                           IntConLevel icl) {

  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(vars[i]); }
  EXCSTART
    distinct(*space, va, icl);
  EXCSTOP
}

void gecode_distinct_ints_ivars(CLSpace *space, const int *intoffsets, const int *vars, 
                                vector<IntVar>::size_type count, 
                                IntConLevel icl) {
  IntVarArgs va(count);
  IntArgs ia(count);
  for(unsigned i=0; i<count; i++) { 
    va[i]=space->getIntVar(vars[i]);
    ia[i] = intoffsets[i];
  }
  EXCSTART
  distinct(*space, ia, va, icl);
  EXCSTOP
}


  /* sorted constraint */

void gecode_sorted_ivars_ivars(CLSpace *space, const  int xvars[], const  int yvars[], 
                         vector<IntVar>::size_type count, IntConLevel icl) {

  IntVarArgs xva(count);
  IntVarArgs yva(count);
  for(unsigned i=0; i<count; i++) {
    xva[i]=space->getIntVar(xvars[i]);
    yva[i]=space->getIntVar(yvars[i]);
  }
  EXCSTART
    sorted(*space, xva, yva, icl);
  EXCSTOP
}

void gecode_sorted_ivars_ivars_ivars(CLSpace *space, const  int xvars[],
                                     const  int yvars[], const  int zvars[], 
                                     vector<IntVar>::size_type count, IntConLevel icl) {

  IntVarArgs xva(count);
  IntVarArgs yva(count);
  IntVarArgs zva(count);
  for(unsigned i=0; i<count; i++) {
    xva[i]=space->getIntVar(xvars[i]);
    yva[i]=space->getIntVar(yvars[i]);
    zva[i]=space->getIntVar(zvars[i]);
  }
  EXCSTART
    sorted(*space, xva, yva, zva, icl);
  EXCSTOP
}


/* channeling constraints */

void gecode_channel_ivars_ivars(CLSpace *space,
                                int v1[], int v2[],
                                vector<IntVar>::size_type count,
                                IntConLevel icl) {
  IntVarArgs v1a(count);
  IntVarArgs v2a(count);
  for(unsigned i=0; i<count; i++)
    { v1a[i]=space->getIntVar(v1[i]);
      v2a[i]=space->getIntVar(v2[i]); }
  channel(*space, v1a, v2a, icl);
}

void gecode_channel_ivars_int_ivars_int(CLSpace *space,
                                        int v1[],
                                        int xoff,
                                        int v2[],
                                        int yoff,
                                        vector<IntVar>::size_type count,
                                        IntConLevel icl) {
  IntVarArgs v1a(count);
  IntVarArgs v2a(count);
  for(unsigned i=0; i<count; i++)
    { v1a[i]=space->getIntVar(v1[i]);
      v2a[i]=space->getIntVar(v2[i]); }
  channel(*space, v1a, xoff, v2a, yoff, icl);
}

void gecode_channel_bvar_ivar(CLSpace *space,
                              vector<BoolVar>::size_type bvar,
                              vector<IntVar>::size_type ivar,
                              IntConLevel icl) {
  channel(*space, space->getBoolVar(bvar), space->getIntVar(ivar), icl);
}

void gecode_channel_ivar_bvar(CLSpace *space,
                              vector<IntVar>::size_type ivar,
                              vector<BoolVar>::size_type bvar,
                              IntConLevel icl) {
  channel(*space, space->getIntVar(ivar), space->getBoolVar(bvar), icl);
}

void gecode_channel_bvars_ivar_int(CLSpace *space,
                                   int v1[], vector<BoolVar>::size_type count,
                                   vector<IntVar>::size_type ivar, int offset,
                                   IntConLevel icl) {
  BoolVarArgs v1a(count);
  for(unsigned i=0; i<count; i++)
    { v1a[i]=space->getBoolVar(v1[i]); }
  channel(*space, v1a, space->getIntVar(ivar), offset, icl);
}

/* integer arithmetic constraints */

void gecode_min_ivar_ivar_ivar(CLSpace *space,
                               vector<IntVar>::size_type x1,
                               vector<IntVar>::size_type x2,
                               vector<IntVar>::size_type ret,
                               IntConLevel icl) {
  min(*space, 
      space->getIntVar(x1), 
      space->getIntVar(x2),
      space->getIntVar(ret),
      icl);
}

void gecode_min_ivars_ivar(CLSpace *space,
                           int v[], vector<IntVar>::size_type count,
                           int y,
                           IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  min(*space, va, space->getIntVar(y), icl);
}

void gecode_max_ivar_ivar_ivar(CLSpace *space,
                               vector<IntVar>::size_type x1,
                               vector<IntVar>::size_type x2,
                               vector<IntVar>::size_type ret,
                               IntConLevel icl) {
  max(*space, 
      space->getIntVar(x1), 
      space->getIntVar(x2),
      space->getIntVar(ret),
      icl);
}

void gecode_max_ivars_ivar(CLSpace *space,
                           int v[], vector<IntVar>::size_type count,
                           int y,
                           IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  max(*space, va, space->getIntVar(y), icl);
}

void gecode_abs_ivar_ivar(CLSpace *space,
                          vector<IntVar>::size_type x1,
                          vector<IntVar>::size_type x2,
                          IntConLevel icl) {
  abs(*space, 
      space->getIntVar(x1), 
      space->getIntVar(x2),
      icl);
}

void gecode_mul_ivar_ivar_ivar(CLSpace *space,
                               vector<IntVar>::size_type x1,
                               vector<IntVar>::size_type x2,
                               vector<IntVar>::size_type ret,
                               IntConLevel icl) {
  mult(*space, 
       space->getIntVar(x1), 
       space->getIntVar(x2),
       space->getIntVar(ret),
       icl);
}

void gecode_sqr_ivar_ivar(CLSpace *space,
                          vector<IntVar>::size_type x1,
                          vector<IntVar>::size_type x2,
                          IntConLevel icl) {
  sqr(*space, 
      space->getIntVar(x1), 
      space->getIntVar(x2),
      icl);
}

void gecode_sqrt_ivar_ivar(CLSpace *space,
                           vector<IntVar>::size_type x1,
                           vector<IntVar>::size_type x2,
                           IntConLevel icl) {
  sqrt(*space, 
       space->getIntVar(x1), 
       space->getIntVar(x2),
       icl);
}

void gecode_divmod_ivar_ivar_ivar_ivar(CLSpace *space,
                                       vector<IntVar>::size_type x0,
                                       vector<IntVar>::size_type x1,
                                       vector<IntVar>::size_type x2,
                                       vector<IntVar>::size_type x3,
                                       IntConLevel icl) {
  divmod(*space, 
         space->getIntVar(x0), 
         space->getIntVar(x1), 
         space->getIntVar(x2),
         space->getIntVar(x3),
         icl);
}

void gecode_div_ivar_ivar_ivar(CLSpace *space,
                               vector<IntVar>::size_type x1,
                               vector<IntVar>::size_type x2,
                               vector<IntVar>::size_type ret,
                               IntConLevel icl) {
  div(*space, 
      space->getIntVar(x1), 
      space->getIntVar(x2),
      space->getIntVar(ret),
      icl);
}

void gecode_mod_ivar_ivar_ivar(CLSpace *space,
                               vector<IntVar>::size_type x1,
                               vector<IntVar>::size_type x2,
                               vector<IntVar>::size_type ret,
                               IntConLevel icl) {
  mod(*space, 
      space->getIntVar(x1), 
      space->getIntVar(x2),
      space->getIntVar(ret),
      icl);
}


/* integer linear constraint */

void gecode_lin_ivars_int(CLSpace *space, IntRelType rel, int v[],
                          vector<IntVar>::size_type count, int value,
                          IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, va, rel, value, icl);
}

void gecode_lin_ivars_ivar(CLSpace *space, IntRelType rel, int v[],
                          vector<IntVar>::size_type count, int var,
                          IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, va, rel, space->getIntVar(var), icl);
}

void gecode_lin_ivars_int_reified(CLSpace *space, IntRelType rel, int v[],
                                  vector<IntVar>::size_type count, int value,
                                  ReifyMode mode, int bvar, IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, va, rel, value, Reify(space->getBoolVar(bvar), mode), icl);
}

void gecode_lin_ivars_ivar_reified(CLSpace *space, IntRelType rel, int v[],
                                   vector<IntVar>::size_type count, int var,
                                   ReifyMode mode, int bvar, IntConLevel icl) {
  IntVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, va, rel, space->getIntVar(var), Reify(space->getBoolVar(bvar),mode), icl);
}

void gecode_lin_ints_ivars_int(CLSpace *space, IntRelType rel, int ints[], int v[],
                               vector<IntVar>::size_type count, int value,
                               IntConLevel icl) {
  IntVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, ia, va, rel, value, icl);
}

void gecode_lin_ints_ivars_ivar(CLSpace *space, IntRelType rel, int ints[], int v[],
                                vector<IntVar>::size_type count, int var,
                                IntConLevel icl) {
  IntVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, ia, va, rel, space->getIntVar(var), icl);
}

void gecode_lin_ints_ivars_int_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                       vector<IntVar>::size_type count, int value,
                                       ReifyMode mode, int bvar, IntConLevel icl) {
  IntVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, ia, va, rel, value, Reify(space->getBoolVar(bvar), mode), icl);
}

void gecode_lin_ints_ivars_ivar_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                        vector<IntVar>::size_type count, int var,
                                        ReifyMode mode, int bvar, IntConLevel icl) {
  IntVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getIntVar(v[i]); }
  linear(*space, ia, va, rel, space->getIntVar(var), Reify(space->getBoolVar(bvar), mode), icl);
}

/* boolean linear constraint */

void gecode_lin_bvars_int(CLSpace *space, IntRelType rel, int v[],
                          vector<BoolVar>::size_type count, int value,
                          IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, va, rel, value, icl);
}

void gecode_lin_bvars_ivar(CLSpace *space, IntRelType rel, int v[],
                          vector<BoolVar>::size_type count, int var,
                          IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, va, rel, space->getIntVar(var), icl);
}

void gecode_lin_bvars_int_reified(CLSpace *space, IntRelType rel, int v[],
                                  vector<BoolVar>::size_type count, int value,
                                  ReifyMode mode, int bvar, IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, va, rel, value, Reify(space->getBoolVar(bvar), mode), icl);
}

void gecode_lin_bvars_ivar_reified(CLSpace *space, IntRelType rel, int v[],
                                   vector<BoolVar>::size_type count, int var,
                                   ReifyMode mode, int bvar, IntConLevel icl) {
  BoolVarArgs va(count);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, va, rel, space->getIntVar(var), Reify(space->getBoolVar(bvar), mode), icl);
}

void gecode_lin_ints_bvars_int(CLSpace *space, IntRelType rel, int ints[], int v[],
                               vector<BoolVar>::size_type count, int value,
                               IntConLevel icl) {
  BoolVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, ia, va, rel, value, icl);
}

void gecode_lin_ints_bvars_ivar(CLSpace *space, IntRelType rel, int ints[], int v[],
                                vector<BoolVar>::size_type count, int var,
                                IntConLevel icl) {
  BoolVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, ia, va, rel, space->getIntVar(var), icl);
}

void gecode_lin_ints_bvars_int_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                       vector<BoolVar>::size_type count, int value,
                                       ReifyMode mode, int bvar, IntConLevel icl) {
  BoolVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, ia, va, rel, value, Reify(space->getBoolVar(bvar), mode), icl);
}

void gecode_lin_ints_bvars_ivar_reified(CLSpace *space, IntRelType rel, int ints[], int v[],
                                        vector<BoolVar>::size_type count, int var,
                                        ReifyMode mode, int bvar, IntConLevel icl) {
  BoolVarArgs va(count);
  IntArgs ia(count, ints);
  for(unsigned i=0; i<count; i++)
    { va[i]=space->getBoolVar(v[i]); }
  linear(*space, ia, va, rel, space->getIntVar(var), Reify(space->getBoolVar(bvar), mode), icl);
}


} /* extern "C" */



/*
#include <stdio.h>

int main(void) {
  CLSpace *space = gecode_space_create();
  STATUS status;

  Search::Options options = Search::Options();

  unsigned x = gecode_int_addvar(space, 1, 3);
  unsigned y = gecode_int_addvar(space, 3, 3);
  int min, max, size;

  gecode_intClChannel(space, x, 3);
  DFS<CLSpace> *dfs = new DFS<CLSpace>(space, options);
  status = gecode_get_int_info(space, x, &min, &max, &size);

  
  printf("status 1 of x; (status, min, max, size): %d, %d, %d, %d\n", status, min, max, size);
  status = gecode_get_int_info(space, y, &min, &max, &size);
  printf("status 1 of y; (status, min, max, size): %d, %d, %d, %d\n", status, min, max, size);

  printf("space failed before < post: %d\n", space->failed());
  gecode_int_rel(space, IRT_LE, x, y);
  printf("space failed after < post: %d\n", space->failed());
  status = gecode_get_int_info(space, x, &min, &max, &size);
  printf("status 2 of x; (status, min, max, size): %d, %d, %d, %d\n", status, min, max, size);
  status = gecode_get_int_info(space, y, &min, &max, &size);
  printf("status 2 of y; (status, min, max, size): %d, %d, %d, %d\n", status, min, max, size);
  

  return 0;
}
*/
