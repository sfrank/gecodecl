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
    return (boolVariables[var]);
  }

  BoolVar* getBoolVarp(vector<BoolVar>::size_type var) {
    return &(boolVariables[var]);
  }

  vector<BoolVar>::size_type inline getBoolVarSize(void) {
    return boolVariables.size();
  }

  STATUS inline getBoolInfo(BoolVar* var, int *value) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      return STATE_FAILED;
    }

    if (var->none()) {
      return VAR_UNASSIGNED;
    } else {
      *value = var->val();
      return VAR_ASSIGNED;
    }
  }

  // integer variables

  vector<IntVar>::size_type addIntVariable(IntVar var) {
    //IntVar var(*this, min, max);
    intVariables.push_back(var);
    return intVariables.size() - 1;
  }

  IntVar getIntVar(vector<IntVar>::size_type var) const {
    return (intVariables[var]);
  }

  IntVar* getIntVarp(vector<IntVar>::size_type var) {
    return &(intVariables[var]);
  }

  vector<IntVar>::size_type inline getIntVarSize(void) {
    return intVariables.size();
  }

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

  vector<FloatVar>::size_type addFloatVariable(double min, double max) {
    FloatVar var(*this, min, max);
    floatVariables.push_back(var);
    return floatVariables.size() - 1;
  }

  FloatVar inline getFloatVar(vector<FloatVar>::size_type var) const {
    return (floatVariables[var]);
  }

  FloatVar* getFloatVarp(vector<FloatVar>::size_type var) {
    return &(floatVariables[var]);
  }

  vector<FloatVar>::size_type inline getFloatVarSize(void) {
    return floatVariables.size();
  }
 
  STATUS inline getFloatInfo(FloatVar* var, double *min, double *max, double *med) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      *med = 0.0;
      *min = 0.0;
      *max = 0.0;
      return STATE_FAILED;
    }

    *min = var->min();
    *max = var->max();
    *med = var->med();
    if (var->size() == 0.0) {
      return VAR_ASSIGNED;
    }
    else {
      return VAR_UNASSIGNED;
    }
  }

  /*  
  vector<SetVar>::size_type addSetVariable_full(int glbMin,int glbMax,
                                                int lubMin,int lubMax,
                                                unsigned int cardMin,
                                                unsigned int cardMax) {
    SetVar var(*this,glbMin, glbMax, lubMin, lubMax, cardMin, cardMax);
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
  }
  
  CLIntArgs(int n) : IntArgs(n){};
  ~CLIntArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  int *adr(void) {
    return a;
  }
};

class CLFloatArgs : public FloatArgs {
public:
  static void* operator new(size_t) {
    return ::operator new(sizeof(CLFloatArgs));
  }

  static void  operator delete(void* v) {
    ::operator delete(v);
  }

  CLFloatArgs(int n) : FloatArgs(n){};
  ~CLFloatArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  FloatVal *adr(void) {
    return a;
  }

};

#include "gecodeglue.h"

extern "C" {
  
CLVarArgs* gecode_varargs_create(int n) {
    return new CLVarArgs(n); 
}
void gecode_varargs_set(CLVarArgs *v, int i, const IntVar* e) {
  v->set(i, e);
}
void gecode_varargs_delete(CLVarArgs *v) {
  delete v; }

CLIntArgs* gecode_intargs_create(int n) {
    return new CLIntArgs(n); 
}
int* gecode_intargs_adr(CLIntArgs *v) {
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

CLFloatArgs* gecode_floatargs_create(int n) {
    return new CLFloatArgs(n); 
}
void gecode_floatargs_set(CLFloatArgs *v, int i, double e) {
  v->adr()[i] = FloatVal(e);
}
void gecode_floatargs_delete(CLFloatArgs *v) {
  delete v; }



CLSpace* gecode_space_create(void) { return new CLSpace(); }

void gecode_space_delete(CLSpace *space) { delete space; }

CLSpace* gecode_space_copy(CLSpace *space) {
  return (CLSpace *) space->clone(false); }

SpaceStatus gecode_space_status(CLSpace *space){
  return space->status(); }

size_t gecode_bool_addvar(CLSpace *space) {
  return space->addBoolVariable(); }

size_t gecode_int_addvar(CLSpace *space, int min, int max) {
  return space->addIntVariable(IntVar(*space, min, max)); }

size_t gecode_int_addvar_set(CLSpace *space, IntSet* set) {
  return space->addIntVariable(IntVar(*space, *set)); }


size_t gecode_float_addvar(CLSpace *space, double min, double max) {
  return space->addFloatVariable(min, max); }


BoolVar* gecode_get_boolvar_by_index(CLSpace *space, size_t index) {
  return space->getBoolVarp(index);
}

IntVar* gecode_get_intvar_by_index(CLSpace *space, size_t index) {
  return space->getIntVarp(index);
}

FloatVar* gecode_get_floatvar_by_index(CLSpace *space, size_t index) {
  return space->getFloatVarp(index);
}


STATUS gecode_get_bool_info(CLSpace *space, BoolVar* var, int *value) {
  return space->getBoolInfo(var, value); }

STATUS gecode_get_int_info(CLSpace *space, IntVar* var,
                           int *min, int *max, int *size) {
  return space->getIntInfo(var, min, max, size); }

STATUS gecode_get_float_info(CLSpace *space, FloatVar* var,
                           double *min, double *max, double *median) {
  return space->getFloatInfo(var, min, max, median); }


// Branchers
void gecode_branch_ivar(CLSpace *space, IntVar* var, IntValBranch* valb) {
  branch(*space, *var, *valb);
}

void gecode_branch_int_vars(CLSpace *space, IntVarArgs* vars,
                            IntVarBranch* varb, IntValBranch* valb) {
  branch(*space, *vars, *varb, *valb);
}

void gecode_branch_bvar(CLSpace *space, BoolVar* var, IntValBranch* valb) {
  branch(*space, *var, *valb);
}

void gecode_branch_bool_vars(CLSpace *space, BoolVarArgs* vars,
                            IntVarBranch* varb, IntValBranch* valb) {
  branch(*space, *vars, *varb, *valb);
}

/* variable selectors for branchers */
void gecode_ivar_selector_delete(IntVarBranch* s){
  delete s;
}

IntVarBranch* INT_VAR_NONE(void){
  // Gecode::INT_VAR_NONE();
  return new IntVarBranch(IntVarBranch::SEL_NONE,NULL);
}

IntVarBranch* INT_VAR_RND(unsigned int seed){
  // Gecode::INT_VAR_RND(Rnd(seed));
  return new IntVarBranch(Rnd(seed));
}
/* TODO: check whether to insert MERIT variants */
IntVarBranch* INT_VAR_DEGREE_MIN(void){
  // Gecode::INT_VAR_DEGREE_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_DEGREE_MIN, NULL);
}
IntVarBranch* INT_VAR_DEGREE_MAX(void){
  // Gecode::INT_VAR_DEGREE_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_DEGREE_MAX, NULL);
}
IntVarBranch* INT_VAR_AFC_MIN(void){
  // Gecode::INT_VAR_AFC_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_AFC_MIN, NULL);
}
IntVarBranch* INT_VAR_AFC_MAX(void){
  // Gecode::INT_VAR_AFC_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_AFC_MAX, NULL);
}
/* TODO: check whether to insert ACTIVITY variants */
/*
IntVarBranch* INT_VAR_ACTIVITY_MIN_ints
  (CLSpace *space, IntVarArgs* vars, double d)
{
  return new INT_VAR_ACTIVITY_MIN(IntActivity(*space,*v,d));
}
*/
IntVarBranch* INT_VAR_MIN_MIN(void){
  // Gecode::INT_VAR_MIN_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_MIN_MIN, NULL);
}
IntVarBranch* INT_VAR_MIN_MAX(void){
  // Gecode::INT_VAR_MIN_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_MIN_MAX, NULL);
}
IntVarBranch* INT_VAR_MAX_MIN(void){
  // Gecode::INT_VAR_MAX_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_MAX_MIN, NULL);
}
IntVarBranch* INT_VAR_MAX_MAX(void){
  // Gecode::INT_VAR_MAX_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_MAX_MAX, NULL);
}
IntVarBranch* INT_VAR_SIZE_MIN(void){
  // Gecode::INT_VAR_SIZE_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_MIN, NULL);
}
IntVarBranch* INT_VAR_SIZE_MAX(void){
  // Gecode::INT_VAR_SIZE_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_MAX, NULL);
}
IntVarBranch* INT_VAR_SIZE_DEGREE_MIN(void){
  // Gecode::INT_VAR_SIZE_DEGREE_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_MAX, NULL);
}
IntVarBranch* INT_VAR_SIZE_DEGREE_MAX(void){
  // Gecode::INT_VAR_SIZE_DEGREE_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_DEGREE_MIN, NULL);
}
IntVarBranch* INT_VAR_SIZE_AFC_MIN(void){
  // Gecode::INT_VAR_SIZE_AFC_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_AFC_MIN, NULL);
}
IntVarBranch* INT_VAR_SIZE_AFC_MAX(void){
  // Gecode::INT_VAR_SIZE_AFC_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_AFC_MAX, NULL);
}
/* TODO: check whether to insert ACTIVITY variants */
IntVarBranch* INT_VAR_REGRET_MIN_MIN(void){
  // Gecode::INT_VAR_REGRET_MIN_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_REGRET_MIN_MIN, NULL);
}
IntVarBranch* INT_VAR_REGRET_MIN_MAX(void){
  // Gecode::INT_VAR_REGRET_MIN_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_REGRET_MIN_MAX, NULL);
}
IntVarBranch* INT_VAR_REGRET_MAX_MIN(void){
  // Gecode::INT_VAR_REGRET_MAX_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_REGRET_MAX_MIN, NULL);
}
IntVarBranch* INT_VAR_REGRET_MAX_MAX(void){
  // Gecode::INT_VAR_REGRET_MAX_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_REGRET_MAX_MAX, NULL);
}

/* value selectors for branchers */
void gecode_ival_selector_delete(IntValBranch* s){
  delete s;
}
IntValBranch* INT_VAL_MIN(void){
  // Gecode::INT_VAL_MIN();
  return new IntValBranch(IntValBranch::SEL_MIN);
}
IntValBranch* INT_VAL_MED(void){
  // Gecode::INT_VAL_MED();
  return new IntValBranch(IntValBranch::SEL_MED);
}
IntValBranch* INT_VAL_MAX(void){
  // Gecode::INT_VAL_MAX();
  return new IntValBranch(IntValBranch::SEL_MAX);
}
IntValBranch* INT_VAL_RND(unsigned int seed){
  // Gecode::INT_VAL_RND(Rnd(seed));
  return new IntValBranch(Rnd(seed));
}
IntValBranch* INT_VAL_SPLIT_MIN(void){
  // Gecode::INT_VAL_SPLIT_MIN();
  return new IntValBranch(IntValBranch::SEL_SPLIT_MIN);
}
IntValBranch* INT_VAL_SPLIT_MAX(void){
  // Gecode::INT_VAL_SPLIT_MAX();
  return new IntValBranch(IntValBranch::SEL_SPLIT_MAX);
}
IntValBranch* INT_VAL_RANGE_MIN(void){
  // Gecode::INT_VAL_RANGE_MIN();
  return new IntValBranch(IntValBranch::SEL_RANGE_MIN);
}
IntValBranch* INT_VAL_RANGE_MAX(void){
  // Gecode::INT_VAL_RANGE_MAX();
  return new IntValBranch(IntValBranch::SEL_RANGE_MAX);
}
IntValBranch* INT_VALUES_MIN(void){
  // Gecode::INT_VALUES_MIN();
  return new IntValBranch(IntValBranch::SEL_VALUES_MIN);
}
IntValBranch* INT_VALUES_MAX(void){
  // Gecode::INT_VALUES_MAX();
  return new IntValBranch(IntValBranch::SEL_VALUES_MAX);
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
  EXCSTART
    return dfs->next();
  EXCSTOP
    return NULL;
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
                         BoolVar* v, int value, IntConLevel icl) {
  rel(*space, *v, op, value, icl); }

void gecode_rel_bvar_bvar(CLSpace *space, IntRelType op,
                          BoolVar* v1, BoolVar* v2, IntConLevel icl) {
  rel(*space, *v1, op, *v2, icl); }

void gecode_rel_bvar_int_reified(CLSpace *space, IntRelType op, BoolVar* v1,
                                 int val, ReifyMode mode, BoolVar* v2, IntConLevel icl) {
  rel(*space, *v1, op, val, Reify(*v2, mode), icl); }

void gecode_rel_bvar_bvar_reified(CLSpace *space, IntRelType op, BoolVar* v1,
                                  BoolVar* v2, ReifyMode mode, BoolVar* b1, IntConLevel icl) {
  rel(*space, *v1, op, *v2,
      Reify(*b1, mode), icl);
}

void gecode_rel_bvars(CLSpace *space, IntRelType op, BoolVarArgs* v,
                      IntConLevel icl) {
  rel(*space, *v, op, icl);
}

void gecode_rel_bvars_int(CLSpace *space, IntRelType op, BoolVarArgs* v,
                          int value, IntConLevel icl) {
  rel(*space, *v, op, value, icl);
}

void gecode_rel_bvars_bvars(CLSpace *space, IntRelType op,
                            BoolVarArgs* v1, BoolVarArgs* v2,
                            IntConLevel icl) {
  rel(*space, *v1, op, *v2, icl);
}


/* boolean operations */

void gecode_op_bvar_bvar_bvar(CLSpace *space, BoolOpType op, BoolVar* v1,
                              BoolVar* v2, BoolVar* b1, IntConLevel icl) {
  rel(*space, *v1, op, *v2, *b1, icl);
}

void gecode_op_bvar_bvar_int(CLSpace *space, BoolOpType op, BoolVar* v1,
                             BoolVar* v2, int n, IntConLevel icl) {
  rel(*space, *v1, op, *v2, n, icl);
}

void gecode_op_bvars_int(CLSpace *space, BoolOpType op, BoolVarArgs* v,
                          int value, IntConLevel icl) {
  rel(*space, op, *v, value, icl);
}

void gecode_op_bvars_bvar(CLSpace *space, BoolOpType op, BoolVarArgs* v,
                          BoolVar* var, IntConLevel icl) {
  rel(*space, op, *v, *var, icl);
}

void gecode_clause_bvars_bvars_int(CLSpace *space, BoolOpType op,
                                   BoolVarArgs* v1, BoolVarArgs* v2,
                                   int n, IntConLevel icl) {
  clause(*space, op, *v1, *v2, n, icl);
}

void gecode_clause_bvars_bvars_bvar(CLSpace *space, BoolOpType op,
                                    BoolVarArgs* v1, BoolVarArgs* v2,
                                    BoolVar* bvar, IntConLevel icl) {
  clause(*space, op, *v1, *v2, *bvar, icl);
}

/* finite domain integer relations */
void gecode_dom_ivar_int(CLSpace *space,
                         IntVar* v, int value, IntConLevel icl) {
  dom(*space, *v, value, icl);
}

void gecode_dom_ivars_int(CLSpace *space,
                          IntVarArgs* v, int value, IntConLevel icl) {
  dom(*space, *v, value, icl);
}

void gecode_dom_ivar_int_int(CLSpace *space, IntVar* v,
                             int l, int m, IntConLevel icl) {
  dom(*space, *v, l, m, icl);
}

void gecode_dom_ivars_int_int(CLSpace *space, IntVarArgs* v,
                             int l, int m, IntConLevel icl) {
  dom(*space, *v, l, m, icl);
}

void gecode_dom_ivar_iset(CLSpace *space, IntVar* v, 
                          IntSet* s, IntConLevel icl) {
  dom(*space, *v, *s, icl);
}

void gecode_dom_ivars_iset(CLSpace *space, IntVarArgs* v, 
                          IntSet* s, IntConLevel icl) {
  dom(*space, *v, *s, icl);
}

void gecode_dom_ivar_int_reify(CLSpace *space, IntVar* v, int value,
                               ReifyMode mode, BoolVar* bvar, IntConLevel icl) {
  dom(*space, *v, value, Reify(*bvar, mode), icl);
}

void gecode_dom_ivar_int_int_reify(CLSpace *space, IntVar* v, int l, int m,
                                   ReifyMode mode, BoolVar* bvar, IntConLevel icl) {
    dom(*space, *v, l, m, Reify(*bvar, mode), icl);
}

void gecode_dom_ivar_iset_reify(CLSpace *space, IntVar* v, IntSet* s,
                               ReifyMode mode, BoolVar* bvar, IntConLevel icl) {
  dom(*space, *v, *s, Reify(*bvar, mode), icl);
}


void gecode_rel_ivar_int(CLSpace *space, IntRelType op,
                         IntVar* v, int value, IntConLevel icl) {
  rel(*space, *v, op, value, icl); }

void gecode_rel_ivar_ivar(CLSpace *space, IntRelType op,
                          IntVar* v1, IntVar* v2, IntConLevel icl) {
  rel(*space, *v1, op, *v2, icl); }

void gecode_rel_ivar_int_reified(CLSpace *space, IntRelType op, IntVar* v1,
                                 int val, ReifyMode mode, BoolVar* b1, IntConLevel icl) {
  rel(*space, *v1, op, val, 
      Reify(*b1, mode), icl); }

void gecode_rel_ivar_ivar_reified(CLSpace *space, IntRelType op, IntVar* v1,
                                  IntVar* v2, ReifyMode mode, BoolVar* b1, IntConLevel icl) {
  rel(*space, *v1, op, *v2, Reify(*b1, mode), icl);
}

void gecode_rel_ivars(CLSpace *space, IntRelType op, IntVarArgs* v,
                      IntConLevel icl) {
  rel(*space, *v, op, icl);
}


void gecode_rel_ivars_int(CLSpace *space, IntRelType op, IntVarArgs* v,
                          int value, IntConLevel icl) {
  rel(*space, *v, op, value, icl);
}

void gecode_rel_ivars_ivars(CLSpace *space, IntRelType op,
                            IntVarArgs* v1, IntVarArgs* v2,
                            IntConLevel icl) {
  rel(*space, *v1, op, *v2, icl);
}


/* distinct constraint */
void gecode_distinct_ivars(CLSpace *space, IntVarArgs *va, IntConLevel icl) {
  EXCSTART
    distinct(*space, *va, icl);
  EXCSTOP
}

void gecode_distinct_ints_ivars(CLSpace *space, IntArgs *ia, IntVarArgs *va, IntConLevel icl) {
  EXCSTART
    distinct(*space, *ia, *va, icl);
  EXCSTOP
}


/* sorted constraint */

void gecode_sorted_ivars_ivars(CLSpace *space, IntVarArgs* xvars,
                               IntVarArgs* yvars, IntConLevel icl) {
  EXCSTART
    sorted(*space, *xvars, *yvars, icl);
  EXCSTOP
}

void gecode_sorted_ivars_ivars_ivars(CLSpace *space, IntVarArgs* xvars,
                                     IntVarArgs* yvars, IntVarArgs* zvars, 
                                     IntConLevel icl) {
  EXCSTART
    sorted(*space, *xvars, *yvars, *zvars, icl);
  EXCSTOP
}

/* binpacking constraint */
void gecode_binpacking(CLSpace *space,
                       IntVarArgs* l, IntVarArgs *b, IntArgs* s,
                       IntConLevel icl) {
  EXCSTART
    binpacking(*space, *l, *b, *s, icl);
  EXCSTOP
}

/* nooverlap constraint */
void gecode_nooverlap(CLSpace *space,
                      IntVarArgs *x, IntArgs* w,
                      IntVarArgs *y, IntArgs* h,
                      IntConLevel icl) {
  EXCSTART
    nooverlap(*space, *x, *w, *y, *h, icl);
  EXCSTOP  
}

void gecode_nooverlap_optional(CLSpace *space,
                               IntVarArgs *x, IntArgs* w,
                               IntVarArgs *y, IntArgs* h,
                               BoolVarArgs* o,
                               IntConLevel icl) {
  EXCSTART
    nooverlap(*space, *x, *w, *y, *h, *o, icl);
  EXCSTOP  
}

void gecode_nooverlap_coords(CLSpace *space,
                             IntVarArgs *x0, IntVarArgs* w, IntVarArgs *x1,
                             IntVarArgs *y0, IntVarArgs* h, IntVarArgs *y1,
                             IntConLevel icl) {
  EXCSTART
    nooverlap(*space, *x0, *w, *x1, *y0, *h, *y1, icl);
  EXCSTOP  
}

void gecode_nooverlap_coords_optional(CLSpace *space,
                                      IntVarArgs *x0, IntVarArgs* w, IntVarArgs *x1,
                                      IntVarArgs *y0, IntVarArgs* h, IntVarArgs *y1,
                                      BoolVarArgs* o,
                                      IntConLevel icl) {
  EXCSTART
    nooverlap(*space, *x0, *w, *x1, *y0, *h, *y1, *o, icl);
  EXCSTOP  
}


/* cumulatives constraint */
void gecode_cumulatives_ivars(CLSpace *space,
                              IntVarArgs* m,
                              IntVarArgs* s,
                              IntVarArgs* p,
                              IntVarArgs* e,
                              IntVarArgs* u,
                              IntArgs* c,
                              bool at_most,
                              IntConLevel icl) {
  cumulatives(*space, *m, *s, *p, *e, *u, *c, at_most, icl);
}


/* unary constraint */
void gecode_unary_ivars(CLSpace *space, IntVarArgs* s, IntArgs* p,
                        IntConLevel icl) {
  unary(*space, *s, *p, icl);
}

void gecode_unary_ivars_bvars(CLSpace *space, IntVarArgs* s, IntArgs* p,
                              BoolVarArgs* b, IntConLevel icl) {
  unary(*space, *s, *p, *b, icl);
}

void gecode_unary_tasks_ivars(CLSpace *space, TaskTypeArgs* tasks, 
                              IntVarArgs* flex, IntArgs* fix,
                              IntConLevel icl) {
  unary(*space, *tasks, *flex, *fix, icl);
}

void gecode_unary_tasks_ivars_bvars(CLSpace *space, TaskTypeArgs* tasks, 
                                    IntVarArgs* flex, IntArgs* fix,
                                    BoolVarArgs* m, IntConLevel icl) {
  unary(*space, *tasks, *flex, *fix, *m, icl);
}

void gecode_unary_ivars_ivars(CLSpace *space, IntVarArgs* s,
                              IntVarArgs* p, IntVarArgs* e,
                              IntConLevel icl) {
  unary(*space, *s, *p, *e, icl);
}

void gecode_unary_ivars_ivars_bvars(CLSpace *space, IntVarArgs* s,
                                    IntVarArgs* p, IntVarArgs* e,
                                    BoolVarArgs* m, IntConLevel icl) {
  unary(*space, *s, *p, *e, *m, icl);
}


/* cumulative constraint */
void gecode_cumulative_int_tasks(CLSpace *space, int c,
                                 TaskTypeArgs* t, IntVarArgs* flex,
                                 IntArgs* fix, IntArgs* u,
                                 IntConLevel icl) {
  cumulative(*space, c, *t, *flex, *fix, *u, icl);
}

void gecode_cumulative_ivar_tasks(CLSpace *space, IntVar* c,
                                  TaskTypeArgs* t, IntVarArgs* flex,
                                  IntArgs* fix, IntArgs* u,
                                  IntConLevel icl) {
  cumulative(*space, *c, *t, *flex, *fix, *u, icl);
}

void gecode_cumulative_int_tasks_bvars(CLSpace *space, int c,
                                       TaskTypeArgs* t, IntVarArgs* flex,
                                       IntArgs* fix, IntArgs* u, BoolVarArgs* m,
                                       IntConLevel icl) {
  cumulative(*space, c, *t, *flex, *fix, *u, *m, icl);
}

void gecode_cumulative_ivar_tasks_bvars(CLSpace *space, IntVar* c,
                                       TaskTypeArgs* t, IntVarArgs* flex,
                                       IntArgs* fix, IntArgs* u, BoolVarArgs* m,
                                       IntConLevel icl) {
  cumulative(*space, *c, *t, *flex, *fix, *u, *m, icl);
}

void gecode_cumulative_int(CLSpace *space, int c,
                           IntVarArgs* s, IntArgs* p, IntArgs* u,
                           IntConLevel icl) {
  cumulative(*space, c, *s, *p, *u, icl);
}

void gecode_cumulative_ivar(CLSpace *space, IntVar* c,
                           IntVarArgs* s, IntArgs* p, IntArgs* u,
                           IntConLevel icl) {
  cumulative(*space, *c, *s, *p, *u, icl);
}

void gecode_cumulative_int_bvars(CLSpace *space, int c,
                                 IntVarArgs* s, IntArgs* p, IntArgs* u,
                                 BoolVarArgs* m, IntConLevel icl) {
  cumulative(*space, c, *s, *p, *u, *m, icl);
}

void gecode_cumulative_ivar_bvars(CLSpace *space, IntVar* c,
                                 IntVarArgs* s, IntArgs* p, IntArgs* u,
                                 BoolVarArgs* m, IntConLevel icl) {
  cumulative(*space, *c, *s, *p, *u, *m, icl);
}

void gecode_cumulative_int_ivars(CLSpace *space, int c,
                                 IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                 IntArgs* u, IntConLevel icl) {
  cumulative(*space, c, *s, *p, *e, *u, icl);
}

void gecode_cumulative_ivar_ivars(CLSpace *space, IntVar* c,
                                  IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                  IntArgs* u, IntConLevel icl) {
  cumulative(*space, *c, *s, *p, *e, *u, icl);
}

void gecode_cumulative_int_ivars_bvars(CLSpace *space, int c,
                                       IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                       IntArgs* u, BoolVarArgs* m, IntConLevel icl) {
  cumulative(*space, c, *s, *p, *e, *u, *m, icl);
}

void gecode_cumulative_ivar_ivars_bvars(CLSpace *space, IntVar* c,
                                        IntVarArgs* s, IntVarArgs* p, IntVarArgs* e,
                                        IntArgs* u, BoolVarArgs* m, IntConLevel icl) {
  cumulative(*space, *c, *s, *p, *e, *u, *m, icl);
}


/* channeling constraints */

void gecode_channel_ivars_ivars(CLSpace *space,
                                IntVarArgs* v1, IntVarArgs* v2,
                                IntConLevel icl) {
  channel(*space, *v1, *v2, icl);
}

void gecode_channel_ivars_int_ivars_int(CLSpace *space,
                                        IntVarArgs* v1, int xoff,
                                        IntVarArgs* v2, int yoff,
                                        IntConLevel icl) {
  channel(*space, *v1, xoff, *v2, yoff, icl);
}

void gecode_channel_bvar_ivar(CLSpace *space,
                              BoolVar* bvar, IntVar* ivar,
                              IntConLevel icl) {
  channel(*space, *bvar, *ivar, icl);
}

void gecode_channel_ivar_bvar(CLSpace *space,
                              IntVar* ivar, BoolVar* bvar,
                              IntConLevel icl) {
  channel(*space, *ivar, *bvar, icl);
}

void gecode_channel_bvars_ivar_int(CLSpace *space,
                                   BoolVarArgs* v1, IntVar* ivar, int offset,
                                   IntConLevel icl) {
  channel(*space, *v1, *ivar, offset, icl);
}

/* integer arithmetic constraints */

void gecode_min_ivar_ivar_ivar(CLSpace *space,
                               IntVar* x1, IntVar* x2, IntVar* ret,
                               IntConLevel icl) {
  min(*space, *x1, *x2, *ret, icl);
}

void gecode_min_ivars_ivar(CLSpace *space,
                           IntVarArgs* v, IntVar* y,
                           IntConLevel icl) {
  min(*space, *v, *y, icl);
}

void gecode_max_ivar_ivar_ivar(CLSpace *space,
                               IntVar* x1, IntVar* x2, IntVar* ret,
                               IntConLevel icl) {
  max(*space, *x1, *x2, *ret, icl);
}

void gecode_max_ivars_ivar(CLSpace *space,
                           IntVarArgs* v, IntVar* y,
                           IntConLevel icl) {
  max(*space, *v, *y, icl);
}

void gecode_abs_ivar_ivar(CLSpace *space,
                          IntVar* x1, IntVar* x2,
                          IntConLevel icl) {
  abs(*space, *x1, *x2, icl);
}

void gecode_mul_ivar_ivar_ivar(CLSpace *space,
                               IntVar* x1, IntVar* x2, IntVar* ret,
                               IntConLevel icl) {
  mult(*space, *x1, *x2, *ret, icl);
}

void gecode_sqr_ivar_ivar(CLSpace *space,
                          IntVar* x1, IntVar* x2,
                          IntConLevel icl) {
  sqr(*space, *x1, *x2, icl);
}

void gecode_sqrt_ivar_ivar(CLSpace *space,
                           IntVar* x1, IntVar* x2,
                           IntConLevel icl) {
  sqrt(*space, *x1, *x2, icl);
}

void gecode_divmod_ivar_ivar_ivar_ivar(CLSpace *space,
                                       IntVar* x0,
                                       IntVar* x1,
                                       IntVar* x2,
                                       IntVar* x3,
                                       IntConLevel icl) {
  divmod(*space, *x0, *x1, *x2, *x3, icl);
}

void gecode_div_ivar_ivar_ivar(CLSpace *space,
                               IntVar* x1, IntVar* x2, IntVar* ret,
                               IntConLevel icl) {
  div(*space, *x1, *x2, *ret, icl);
}

void gecode_mod_ivar_ivar_ivar(CLSpace *space,
                               IntVar* x1, IntVar* x2, IntVar* ret,
                               IntConLevel icl) {
  mod(*space, *x1, *x2, *ret, icl);
}


/* integer linear constraint */

void gecode_lin_ivars_int(CLSpace *space, IntRelType rel, IntVarArgs* v,
                          int value, IntConLevel icl) {
  linear(*space, *v, rel, value, icl);
}

void gecode_lin_ivars_ivar(CLSpace *space, IntRelType rel, IntVarArgs* v,
                          IntVar* var, IntConLevel icl) {
  linear(*space, *v, rel, *var, icl);
}

void gecode_lin_ivars_int_reified(CLSpace *space, IntRelType rel, IntVarArgs* v,
                                  int value, ReifyMode mode,
                                  BoolVar* bvar, IntConLevel icl) {
  linear(*space, *v, rel, value, Reify(*bvar, mode), icl);
}

void gecode_lin_ivars_ivar_reified(CLSpace *space, IntRelType rel, IntVarArgs* v,
                                   IntVar* var, ReifyMode mode,
                                   BoolVar* bvar, IntConLevel icl) {
  linear(*space, *v, rel, *var, Reify(*bvar,mode), icl);
}

void gecode_lin_ints_ivars_int(CLSpace *space, IntRelType rel, IntArgs* ints,
                               IntVarArgs* v, int value, IntConLevel icl) {
  linear(*space, *ints, *v, rel, value, icl);
}

void gecode_lin_ints_ivars_ivar(CLSpace *space, IntRelType rel, IntArgs* ints,
                                IntVarArgs* v, IntVar* var, IntConLevel icl) {
  linear(*space, *ints, *v, rel, *var, icl);
}

void gecode_lin_ints_ivars_int_reified(CLSpace *space, IntRelType rel, IntArgs* ints,
                                       IntVarArgs* v, int value,
                                       ReifyMode mode, BoolVar* bvar, IntConLevel icl) {
  linear(*space, *ints, *v, rel, value, Reify(*bvar, mode), icl);
}

void gecode_lin_ints_ivars_ivar_reified(CLSpace *space, IntRelType rel, IntArgs* ints,
                                        IntVarArgs* v, IntVar* var,
                                        ReifyMode mode, BoolVar* bvar, IntConLevel icl) {
  linear(*space, *ints, *v, rel, *var, Reify(*bvar, mode), icl);
}

/* boolean linear constraint */

void gecode_lin_bvars_int(CLSpace *space, IntRelType rel, BoolVarArgs* v,
                          int value, IntConLevel icl) {
  linear(*space, *v, rel, value, icl);
}

void gecode_lin_bvars_ivar(CLSpace *space, IntRelType rel, BoolVarArgs* v,
                          IntVar* var, IntConLevel icl) {
  linear(*space, *v, rel, *var, icl);
}

void gecode_lin_bvars_int_reified(CLSpace *space, IntRelType rel, BoolVarArgs* v,
                                  int value, ReifyMode mode, BoolVar* bvar,
                                  IntConLevel icl) {
  linear(*space, *v, rel, value, Reify(*bvar, mode), icl);
}

void gecode_lin_bvars_ivar_reified(CLSpace *space, IntRelType rel, BoolVarArgs* v,
                                   IntVar* var, ReifyMode mode, BoolVar* bvar,
                                   IntConLevel icl) {
  linear(*space, *v, rel, *var, Reify(*bvar, mode), icl);
}

void gecode_lin_ints_bvars_int(CLSpace *space, IntRelType rel, IntArgs* ints,
                               BoolVarArgs* v, int value, IntConLevel icl) {
  linear(*space, *ints, *v, rel, value, icl);
}

void gecode_lin_ints_bvars_ivar(CLSpace *space, IntRelType rel, IntArgs* ints,
                                BoolVarArgs* v, IntVar* var, IntConLevel icl) {
  linear(*space, *ints, *v, rel, *var, icl);
}

void gecode_lin_ints_bvars_int_reified(CLSpace *space, IntRelType rel, IntArgs* ints,
                                       BoolVarArgs* v, int value, ReifyMode mode,
                                       BoolVar* bvar, IntConLevel icl) {
  linear(*space, *ints, *v, rel, value, Reify(*bvar, mode), icl);
}

void gecode_lin_ints_bvars_ivar_reified(CLSpace *space, IntRelType rel, IntArgs* ints,
                                        BoolVarArgs* v, IntVar* var, ReifyMode mode,
                                        BoolVar* bvar, IntConLevel icl) {
  linear(*space, *ints, *v, rel, *var, Reify(*bvar, mode), icl);
}


/* float domain specific functions */

void gecode_rel_fvar_fvar(CLSpace *space, FloatRelType op, FloatVar* x0, FloatVar* x1) {
  rel(*space, *x0, op, *x1); 
}

void gecode_rel_fvar_dbl(CLSpace *space, FloatRelType op, FloatVar* x0, double x1) {
  rel(*space, *x0, op, FloatVal(x1)); 
}

void gecode_rel_fvar_dbl_reified(CLSpace *space, FloatRelType op, FloatVar* x0, double x1,
				 ReifyMode mode, BoolVar* bvar) {
  rel(*space, *x0, op, FloatVal(x1), Reify(*bvar, mode)); 
}

void gecode_rel_fvar_fvar_reified(CLSpace *space, FloatRelType op, FloatVar* x0, FloatVar* x1,
				  ReifyMode mode, BoolVar* bvar) {
  rel(*space, *x0, op, *x1, Reify(*bvar, mode)); 
}

void gecode_rel_fvars_dbl(CLSpace *space, FloatRelType op, FloatVarArgs* x0, double x1) {
  rel(*space, *x0, op, FloatVal(x1)); 
}

void gecode_rel_fvars_fvar(CLSpace *space, FloatRelType op, FloatVarArgs* x0, FloatVar* x1) {
  rel(*space, *x0, op, *x1); 
}

void gecode_min_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2) {
  min(*space, *x0, *x1, *x2); 
}

void gecode_min_fvars_fvar(CLSpace *space, FloatVarArgs* x, FloatVar* y) {
  min(*space, *x, *y); 
}

void gecode_max_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2) {
  max(*space, *x0, *x1, *x2); 
}

void gecode_max_fvars_fvar(CLSpace *space, FloatVarArgs* x, FloatVar* y) {
  max(*space, *x, *y); 
}

void gecode_abs_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
  abs(*space, *x0, *x1); 
}

void gecode_mult_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2) {
  mult(*space, *x0, *x1, *x2); 
}

void gecode_sqr_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
  sqr(*space, *x0, *x1); 
}

void gecode_sqrt_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
  sqrt(*space, *x0, *x1); 
}

void gecode_pow_fvar_uint_fvar(CLSpace *space, FloatVar* x0, unsigned int pow, FloatVar* x1) {
  Gecode::pow(*space, *x0, pow, *x1); 
}

void gecode_nroot_fvar_uint_fvar(CLSpace *space, FloatVar* x0, unsigned int pow, FloatVar* x1) {
  nroot(*space, *x0, pow, *x1); 
}

void gecode_div_fvar_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1, FloatVar* x2) {
  div(*space, *x0, *x1, *x2); 
}

void gecode_exp_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
  exp(*space, *x0, *x1); 
}

void gecode_log_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
  log(*space, *x0, *x1); 
}

void gecode_pow_dbl_fvar_fvar(CLSpace *space, double base, FloatVar* x0, FloatVar* x1) {
  pow(*space, FloatNum(base), *x0, *x1); 
}

void gecode_log_dbl_fvar_fvar(CLSpace *space, double base, FloatVar* x0, FloatVar* x1) {
  log(*space, FloatNum(base), *x0, *x1); 
}

void gecode_asin_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
  asin(*space, *x0, *x1); 
}

void gecode_sin_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
    sin(*space, *x0, *x1); 
}

void gecode_acos_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
    acos(*space, *x0, *x1); 
}

void gecode_cos_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
    cos(*space, *x0, *x1); 
}

void gecode_atan_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
    atan(*space, *x0, *x1); 
}

void gecode_tan_fvar_fvar(CLSpace *space, FloatVar* x0, FloatVar* x1) {
    tan(*space, *x0, *x1); 
}

void gecode_lin_fvars_dbl(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
                          double c) {
  linear(*space, *x, rel, FloatNum(c));
}

void gecode_lin_fvars_fvar(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
                          FloatVar* y) {
  linear(*space, *x, rel, *y);
}

void gecode_lin_fvars_dbl_reified(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
				  double c, ReifyMode mode, BoolVar* bvar) {
  linear(*space, *x, rel, FloatNum(c), Reify(*bvar, mode));
}

void gecode_lin_fvars_fvar_reified(CLSpace *space, FloatRelType rel, FloatVarArgs* x,
				   FloatVar* y, ReifyMode mode, BoolVar* bvar) {
  linear(*space, *x, rel, *y, Reify(*bvar, mode));
}

void gecode_lin_fargs_fvars_dbl(CLSpace *space, FloatRelType rel, FloatArgs* a, FloatVarArgs* x,
				double c) {
  linear(*space, *a, *x, rel, FloatNum(c));
}

void gecode_lin_fargs_fvars_fvar(CLSpace *space, FloatRelType rel, FloatArgs* a, FloatVarArgs* x,
				 FloatVar* y) {
  linear(*space, *a, *x, rel, *y);
}

void gecode_lin_fargs_fvars_dbl_reified(CLSpace *space, FloatRelType rel, FloatArgs* a,
					FloatVarArgs* x, double c,
					ReifyMode mode, BoolVar* bvar) {
  linear(*space, *a, *x, rel, FloatNum(c), Reify(*bvar, mode));
}

void gecode_lin_fargs_fvars_fvar_reified(CLSpace *space, FloatRelType rel, FloatArgs* a,
					 FloatVarArgs* x, FloatVar* y,
					 ReifyMode mode, BoolVar* bvar) {
  linear(*space, *a, *x, rel, *y, Reify(*bvar, mode));
}

void gecode_channel_fvar_ivar(CLSpace *space, FloatVar* x0, IntVar* x1) {
  channel(*space, *x0, *x1);
}

// Float Branchers
void gecode_branch_fvar(CLSpace *space, FloatVar* var, FloatValBranch* valb) {
  branch(*space, *var, *valb);
}

void gecode_branch_fvars(CLSpace *space, FloatVarArgs* vars,
			 FloatVarBranch* varb, FloatValBranch* valb) {
  branch(*space, *vars, *varb, *valb);
}

/* variable selectors for branchers */
void gecode_fvar_selector_delete(FloatVarBranch* s){
  delete s;
}

FloatVarBranch* FLOAT_VAR_NONE(void){
  // Gecode::FLOAT_VAR_NONE();
  return new FloatVarBranch(FloatVarBranch::SEL_NONE,NULL);
}

FloatVarBranch* FLOAT_VAR_RND(unsigned int seed){
  // Gecode::FLOAT_VAR_RND(Rnd(seed));
  return new FloatVarBranch(Rnd(seed));
}
/* TODO: check whether to insert MERIT variants */
FloatVarBranch* FLOAT_VAR_DEGREE_MIN(void){
  // Gecode::FLOAT_VAR_DEGREE_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_DEGREE_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_DEGREE_MAX(void){
  // Gecode::FLOAT_VAR_DEGREE_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_DEGREE_MAX, NULL);
}
FloatVarBranch* FLOAT_VAR_AFC_MIN(void){
  // Gecode::FLOAT_VAR_AFC_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_AFC_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_AFC_MAX(void){
  // Gecode::FLOAT_VAR_AFC_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_AFC_MAX, NULL);
}
/* TODO: check whether to insert ACTIVITY variants */
/*
FloatVarBranch* FLOAT_VAR_ACTIVITY_MIN_ints
  (CLSpace *space, FloatVarArgs* vars, double d)
{
  return new FLOAT_VAR_ACTIVITY_MIN(FloatActivity(*space,*v,d));
}
*/
FloatVarBranch* FLOAT_VAR_MIN_MIN(void){
  // Gecode::FLOAT_VAR_MIN_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_MIN_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_MIN_MAX(void){
  // Gecode::FLOAT_VAR_MIN_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_MIN_MAX, NULL);
}
FloatVarBranch* FLOAT_VAR_MAX_MIN(void){
  // Gecode::FLOAT_VAR_MAX_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_MAX_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_MAX_MAX(void){
  // Gecode::FLOAT_VAR_MAX_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_MAX_MAX, NULL);
}
FloatVarBranch* FLOAT_VAR_SIZE_MIN(void){
  // Gecode::FLOAT_VAR_SIZE_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_SIZE_MAX(void){
  // Gecode::FLOAT_VAR_SIZE_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_MAX, NULL);
}
FloatVarBranch* FLOAT_VAR_SIZE_DEGREE_MIN(void){
  // Gecode::FLOAT_VAR_SIZE_DEGREE_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_MAX, NULL);
}
FloatVarBranch* FLOAT_VAR_SIZE_DEGREE_MAX(void){
  // Gecode::FLOAT_VAR_SIZE_DEGREE_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_DEGREE_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_SIZE_AFC_MIN(void){
  // Gecode::FLOAT_VAR_SIZE_AFC_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_AFC_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_SIZE_AFC_MAX(void){
  // Gecode::FLOAT_VAR_SIZE_AFC_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_AFC_MAX, NULL);
}
/* TODO: check whether to insert ACTIVITY variants */

/* value selectors for branchers */
void gecode_fval_selector_delete(FloatValBranch* s){
  delete s;
}

FloatValBranch* FLOAT_VAL_SPLIT_MIN(void){
  // Gecode::FLOAT_VAL_SPLIT_MIN();
  return new FloatValBranch(FloatValBranch::SEL_SPLIT_MIN);
}
FloatValBranch* FLOAT_VAL_SPLIT_MAX(void){
  // Gecode::FLOAT_VAL_SPLIT_MAX();
  return new FloatValBranch(FloatValBranch::SEL_SPLIT_MAX);
}
FloatValBranch* FLOAT_VAL_SPLIT_RND(unsigned int seed){
  // Gecode::FLOAT_VAL_SPLIT_RND(Rnd(seed));
  return new FloatValBranch(Rnd(seed));
}


/* finite (integer) sets */

IntSet* gecode_intset_bounds(int min, int max) {
  return new IntSet(min, max);
}

IntSet* gecode_intset_seq(int seq[], int count) {
  return new IntSet(seq, count);
}

IntSet* gecode_intset_ranges(int seq[][2], int count) {
  return new IntSet(seq, count);
}

void gecode_intset_delete(IntSet* iset) {
  delete iset;
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
