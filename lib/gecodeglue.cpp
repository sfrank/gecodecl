#include <vector>
#include <exception>

#include "gecode/kernel.hh"
#include "gecode/support.hh"
#include "gecode/int.hh"
#include "gecode/float.hh"
#include "gecode/set.hh"
#include "gecode/search.hh"
//#include "gecode/minimodel.hh"

#define EXCSTART try {
#define EXCSTOP  }                                 \
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


  // integer set variables
  
  vector<SetVar>::size_type addSetVariable_full(int glbMin,int glbMax,
                                                int lubMin,int lubMax,
                                                unsigned int cardMin,
                                                unsigned int cardMax) {
    SetVar var(*this,glbMin, glbMax, lubMin, lubMax, cardMin, cardMax);
    setVariables.push_back(var);
    return setVariables.size() - 1;
  }

  vector<SetVar>::size_type addSetVariable_sets(IntSet& glbD,
                                                IntSet& lubD,
                                                unsigned int cardMin,
                                                unsigned int cardMax) {
    SetVar var(*this,glbD, lubD, cardMin, cardMax);
    setVariables.push_back(var);
    return setVariables.size() - 1;
  }

  SetVar* getSetVarp(vector<SetVar>::size_type var) {
    return &(setVariables[var]);
  }

  STATUS inline getSetInfo(SetVar* var,
                           int* lubMin, int* lubMax,
                           int* glbMin, int* glbMax,
                           int* cardMin, int* cardMax) {
    SpaceStatus state = status();
    if (state==SS_FAILED) {
      *lubMin = 0;
      *lubMax = 0;
      *glbMin = 0;
      *glbMax = 0;
      *cardMin = 0;
      *cardMax = 0;
      return STATE_FAILED;
    }

    *lubMin = var->lubMin();
    *lubMax = var->lubMax();
    *glbMin = var->glbMin();
    *glbMax = var->glbMax();
    *cardMin = var->cardMin();
    *cardMax = var->cardMax();
    if (var->unknownSize() == 0) {
      return VAR_ASSIGNED;
    }
    else {
      return VAR_UNASSIGNED;
    }
  }

  
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

class CLFloatArgs : public FloatValArgs {
public:
  static void* operator new(size_t) {
    return ::operator new(sizeof(CLFloatArgs));
  }

  static void  operator delete(void* v) {
    ::operator delete(v);
  }

  CLFloatArgs(int n) : FloatValArgs(n){};
  ~CLFloatArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  FloatVal *adr(void) {
    return a;
  }

};

class CLIntSetArgs : public ArgArray<IntSet> {
public:
  static void* operator new(size_t) {
    return ::operator new(sizeof(CLIntSetArgs));
  }

  static void  operator delete(void* v) {
    ::operator delete(v);
  };
  
  CLIntSetArgs(int n) : ArgArray<IntSet>(n){};
  ~CLIntSetArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  void set(int i, const IntSet* e) {
    a[i] = *e;
  }
};

class CLSymmetryHandleArgs : public ArgArray<SymmetryHandle> {
public:
  static void* operator new(size_t) {
    return ::operator new(sizeof(CLSymmetryHandleArgs));
  }

  static void  operator delete(void* v) {
    ::operator delete(v);
  };
  
  CLSymmetryHandleArgs(int n) : ArgArray<SymmetryHandle>(n){};
  ~CLSymmetryHandleArgs(void) {
    if (capacity > onstack_size)
      heap.free(a,capacity);
  }

  void set(int i, const SymmetryHandle& e) {
    a[i] = e;
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
  v->adr()[i] = FloatNum(e);
}
void gecode_floatargs_delete(CLFloatArgs *v) {
  delete v; }


CLIntSetArgs* gecode_intsetargs_create(int n) {
    return new CLIntSetArgs(n); 
}
void gecode_intsetargs_set(CLIntSetArgs *v, int i, const IntSet* e) {
  v->set(i, e);
}
void gecode_intsetargs_delete(CLIntSetArgs *v) {
  delete v; }


CLSymmetryHandleArgs* gecode_symhandleargs_create(int n) {
    return new CLSymmetryHandleArgs(n); 
}
void gecode_symhandleargs_set(CLSymmetryHandleArgs *v, int i, const SymmetryHandle* e) {
  v->set(i, *e);
}
void gecode_symhandleargs_delete(CLSymmetryHandleArgs *v) {
  delete v; }



CLSpace* gecode_space_create(void) { return new CLSpace(); }

void gecode_space_delete(CLSpace *space) { delete space; }

CLSpace* gecode_space_copy(CLSpace *space) {
  return (CLSpace *) space->clone(false); }

unsigned int gecode_space_propagators_count(CLSpace* space) {
  return space->propagators();
}

unsigned int gecode_space_branchers_count(CLSpace* space) {
  return space->branchers();
}

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

size_t gecode_set_addvar_plain(CLSpace *space,
                               int glbMin,int glbMax,
                               int lubMin,int lubMax,
                               unsigned int cardMin,
                               unsigned int cardMax) {
  return space->addSetVariable_full(glbMin, glbMax, lubMin, lubMax, cardMin, cardMax); 
}

size_t gecode_set_addvar_sets(CLSpace *space,
                              IntSet* glbD, IntSet* lubD,
                              unsigned int cardMin,
                              unsigned int cardMax) {
  return space->addSetVariable_sets(*glbD, *lubD, cardMin, cardMax); 
}

BoolVar* gecode_get_boolvar_by_index(CLSpace *space, size_t index) {
  return space->getBoolVarp(index);
}

IntVar* gecode_get_intvar_by_index(CLSpace *space, size_t index) {
  return space->getIntVarp(index);
}

FloatVar* gecode_get_floatvar_by_index(CLSpace *space, size_t index) {
  return space->getFloatVarp(index);
}

SetVar* gecode_get_setvar_by_index(CLSpace *space, size_t index) {
  return space->getSetVarp(index);
}


STATUS gecode_get_bool_info(CLSpace *space, BoolVar* var, int *value) {
  return space->getBoolInfo(var, value); }

STATUS gecode_get_int_info(CLSpace *space, IntVar* var,
                           int *min, int *max, int *size) {
  return space->getIntInfo(var, min, max, size); }

STATUS gecode_get_float_info(CLSpace *space, FloatVar* var,
                           double *min, double *max, double *median) {
  return space->getFloatInfo(var, min, max, median); }

STATUS gecode_get_set_info(CLSpace *space,
                           SetVar* var,
                           int* lubMin, int* lubMax,
                           int* glbMin, int* glbMax,
                           int* cardMin, int* cardMax) {
  return space->getSetInfo(var, lubMin, lubMax, glbMin, glbMax,
                           cardMin, cardMax);
}

// SymmetryHandles
void gecode_symmetryhandle_delete(SymmetryHandle* sh) {
  delete sh;
}


// BrancherHandles
void gecode_brancherhandle_delete(BrancherHandle* bh) {
  delete bh;
}
void gecode_brancherhandle_kill(BrancherHandle* bh, CLSpace* space) {
  bh->kill(*space);
}


// Branchers
BrancherHandle* gecode_branch_ivar(CLSpace *space, IntVar* var, IntValBranch* valb) {
  return new BrancherHandle(branch(*space, *var, *valb));
}

BrancherHandle* gecode_branch_ivars(CLSpace *space, IntVarArgs* vars,
                                    IntVarBranch* varb, IntValBranch* valb) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb));
}

BrancherHandle* gecode_branch_bvar(CLSpace *space, BoolVar* var, IntValBranch* valb) {
  return new BrancherHandle(branch(*space, *var, *valb));
}

BrancherHandle* gecode_branch_bool_vars(CLSpace *space, BoolVarArgs* vars,
                                        IntVarBranch* varb, IntValBranch* valb) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb));
}

// Symmetries
SymmetryHandle* gecode_VariableSymmetry_ivars(IntVarArgs* x) {
  return new SymmetryHandle(VariableSymmetry(*x));
}

SymmetryHandle* gecode_VariableSymmetry_bvars(BoolVarArgs* x) {
  return new SymmetryHandle(VariableSymmetry(*x));
}

SymmetryHandle* gecode_VariableSymmetry_ivars_ints(IntVarArgs* x,
                                                   IntArgs* indices) {
  return new SymmetryHandle(VariableSymmetry(*x, *indices));
}

SymmetryHandle* gecode_ValueSymmetry_ints(IntArgs* x) {
  return new SymmetryHandle(ValueSymmetry(*x));
}

SymmetryHandle* gecode_ValueSymmetry_iset(IntSet* x) {
  return new SymmetryHandle(ValueSymmetry(*x));
}

SymmetryHandle* gecode_ValueSymmetry_ivar(IntVar* var) {
  return new SymmetryHandle(ValueSymmetry(*var));
}

SymmetryHandle* gecode_VariableSequenceSymmetry_ivars_int(IntVarArgs* x, int ss) {
    return new SymmetryHandle(VariableSequenceSymmetry(*x, ss));
}

SymmetryHandle* gecode_VariableSequenceSymmetry_bvars_int(BoolVarArgs* x, int ss) {
    return new SymmetryHandle(VariableSequenceSymmetry(*x, ss));
}

SymmetryHandle* gecode_ValueSequenceSymmetry_ints_int(IntArgs* x, int ss) {
    return new SymmetryHandle(ValueSequenceSymmetry(*x, ss));
}

SymmetryHandle* gecode_values_reflect_int_int(int lower, int upper) {
    return new SymmetryHandle(values_reflect(lower, upper));
}

SymmetryHandle* gecode_values_reflect_ivar(IntVar* x) {
  return new SymmetryHandle(values_reflect(x->min(), x->max()));
}

// Branchers with symmetries
BrancherHandle* gecode_branch_ivars_sym(CLSpace *space, IntVarArgs* vars,
                                        IntVarBranch* varb, IntValBranch* valb,
                                        Symmetries* sym) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb, *sym));
}

BrancherHandle* gecode_branch_bvars_sym(CLSpace *space, BoolVarArgs* vars,
                                        IntVarBranch* varb, IntValBranch* valb,
                                        Symmetries* sym) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb, *sym));
}


/* variable selectors for branchers */
void gecode_ivar_selector_delete(IntVarBranch* s){
  delete s;
}

IntVarBranch* INT_VAR_NONE(void){
  // Gecode::INT_VAR_NONE();
  return new IntVarBranch(IntVarBranch::SEL_NONE, NULL);
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
IntVarBranch* INT_VAR_DEGREE_SIZE_MIN(void){
  // Gecode::INT_VAR_DEGREE_SIZE_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_SIZE_MAX, NULL);
}
IntVarBranch* INT_VAR_DEGREE_SIZE_MAX(void){
  // Gecode::INT_VAR_DEGREE_SIZE_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_DEGREE_SIZE_MIN, NULL);
}
IntVarBranch* INT_VAR_AFC_SIZE_MIN(void){
  // Gecode::INT_VAR_AFC_SIZE_MIN(NULL);
  return new IntVarBranch(IntVarBranch::SEL_AFC_SIZE_MIN, NULL);
}
IntVarBranch* INT_VAR_AFC_SIZE_MAX(void){
  // Gecode::INT_VAR_AFC_SIZE_MAX(NULL);
  return new IntVarBranch(IntVarBranch::SEL_AFC_SIZE_MAX, NULL);
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
IntValBranch* INT_VAL_NEAR_MIN(IntArgs *n){
  // Gecode::INT_VAL_NEAR_MIN(IntSharedArray n);
  return new IntValBranch(IntValBranch::SEL_NEAR_MIN, *n);
}
IntValBranch* INT_VAL_NEAR_MAX(IntArgs *n){
  // Gecode::INT_VAL_NEAR_MAX(IntSharedArray n);
  return new IntValBranch(IntValBranch::SEL_NEAR_MAX, *n);
}
IntValBranch* INT_VAL_NEAR_INC(IntArgs *n){
  // Gecode::INT_VAL_NEAR_INC(IntSharedArray n);
  return new IntValBranch(IntValBranch::SEL_NEAR_INC, *n);
}
IntValBranch* INT_VAL_NEAR_DEC(IntArgs *n){
  // Gecode::INT_VAL_NEAR_DEC(IntSharedArray n);
  return new IntValBranch(IntValBranch::SEL_NEAR_DEC, *n);
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


RBS<DFS, CLSpace> *gecode_rbs_dfs_engine_create(CLSpace *space){
  EXCSTART
    Search::Options options = Search::Options();
    
    // check whether there are any branchers defined at all
    checkBranchers(space);
    return (new RBS<DFS, CLSpace>(space, options));
  EXCSTOP
  return NULL;
}
  
void gecode_rbs_dfs_engine_delete(RBS<DFS, CLSpace> *rbs){ delete rbs;}

CLSpace *gecode_rbs_dfs_engine_next(RBS<DFS, CLSpace> *rbs){
  EXCSTART
    return rbs->next();
  EXCSTOP
    return NULL;
}

RBS<BAB, CLSpace> *gecode_rbs_bab_engine_create(CLSpace *space,
                                                vector<IntVar>::size_type minVar){
  EXCSTART
    space->setCost(minVar);
    Search::Options options = Search::Options(); 
    // check whether there are any branchers defined at all
    checkBranchers(space);
    return (new RBS<BAB, CLSpace>(space, options));
  EXCSTOP
  return NULL;
}
  
void gecode_rbs_bab_engine_delete(RBS<BAB, CLSpace> *rbs){ delete rbs;}

CLSpace *gecode_rbs_bab_engine_next(RBS<BAB, CLSpace> *rbs){
  EXCSTART
    return rbs->next();
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

void gecode_dom_ivar_ivar(CLSpace *space,
                          IntVar* v, IntVar* b, IntConLevel icl) {
  dom(*space, *v, *b, icl);
}

void gecode_dom_bvar_bvar(CLSpace *space,
                          BoolVar* v, BoolVar* b, IntConLevel icl) {
  dom(*space, *v, *b, icl);
}

void gecode_dom_ivars_ivars(CLSpace *space,
                            IntVarArgs* v, IntVarArgs* b, IntConLevel icl) {
  dom(*space, *v, *b, icl);
}

void gecode_dom_bvars_bvars(CLSpace *space,
                            BoolVarArgs* v, BoolVarArgs* b, IntConLevel icl) {
  dom(*space, *v, *b, icl);
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


/* count constraint */
void gecode_count_rel_ivars_int_int(CLSpace *space, IntVarArgs* x,
                                    int n, IntRelType irt, int m,
                                    IntConLevel icl) {
  count(*space, *x, n, irt, m, icl);
}

void gecode_count_rel_ivars_iset_int(CLSpace *space, IntVarArgs* x,
                                     IntSet* y, IntRelType irt, int m,
                                     IntConLevel icl) {
  count(*space, *x, *y, irt, m, icl);
}

void gecode_count_rel_ivars_ivar_int(CLSpace *space, IntVarArgs* x,
                                     IntVar* y, IntRelType irt, int m,
                                     IntConLevel icl) {
  count(*space, *x, *y, irt, m, icl);
}

void gecode_count_rel_ivars_ivars_int(CLSpace *space, IntVarArgs* x,
                                      IntArgs* y, IntRelType irt, int m,
                                      IntConLevel icl) {
  count(*space, *x, *y, irt, m, icl);
}

void gecode_count_rel_ivars_int_ivar(CLSpace *space, IntVarArgs* x,
                                     int n, IntRelType irt, IntVar* z,
                                     IntConLevel icl) {
  count(*space, *x, n, irt, *z, icl);
}

void gecode_count_rel_ivars_iset_ivar(CLSpace *space, IntVarArgs* x,
                                      IntSet* y, IntRelType irt, IntVar* z,
                                      IntConLevel icl) {
  count(*space, *x, *y, irt, *z, icl);
}

void gecode_count_rel_ivars_ivar_ivar(CLSpace *space, IntVarArgs* x,
                                      IntVar* y, IntRelType irt, IntVar* z,
                                      IntConLevel icl) {
  count(*space, *x, *y, irt, *z, icl);
}

void gecode_count_ivars_ivars(CLSpace *space, IntVarArgs* x,
                              IntVarArgs* c, IntConLevel icl) {
  count(*space, *x, *c, icl);
}

void gecode_count_ivars_isets(CLSpace *space, IntVarArgs* x,
                              IntSetArgs* c, IntConLevel icl) {
  count(*space, *x, *c, icl);
}

void gecode_count_ivars_ivars_ints(CLSpace *space, IntVarArgs* x,
                                   IntVarArgs* c, IntArgs* v, IntConLevel icl) {
  count(*space, *x, *c, *v, icl);
}

void gecode_count_ivars_isets_ints(CLSpace *space, IntVarArgs* x,
                                   IntSetArgs* c, IntArgs* v, IntConLevel icl) {
  count(*space, *x, *c, *v, icl);
}

void gecode_count_ivars_iset_ints(CLSpace *space, IntVarArgs* x,
                                  IntSet* c, IntArgs* v, IntConLevel icl) {
  count(*space, *x, *c, *v, icl);
}


/* nvalues constraint */
void gecode_nvalues_ivars_int(CLSpace *space, IntRelType irt,
                              IntVarArgs* x, int y,
                              IntConLevel icl) {
  nvalues(*space, *x, irt, y, icl);
}

void gecode_nvalues_ivars_ivar(CLSpace *space, IntRelType irt,
                               IntVarArgs* x, IntVar* y,
                               IntConLevel icl) {
  nvalues(*space, *x, irt, *y, icl);
}

void gecode_nvalues_bvars_int(CLSpace *space, IntRelType irt,
                              BoolVarArgs* x, int y,
                              IntConLevel icl) {
  nvalues(*space, *x, irt, y, icl);
}

void gecode_nvalues_bvars_ivar(CLSpace *space, IntRelType irt,
                               BoolVarArgs* x, IntVar* y,
                               IntConLevel icl) {
  nvalues(*space, *x, irt, *y, icl);
}


/* sequence constraint */
void gecode_sequence_ivars(CLSpace *space, IntVarArgs* x, IntSet* s,
                           int q, int l, int u, IntConLevel icl) {
  sequence(*space, *x, *s, q, l, u, icl);
}

void gecode_sequence_bvars(CLSpace *space, BoolVarArgs* x, IntSet* s,
                           int q, int l, int u, IntConLevel icl) {
  sequence(*space, *x, *s, q, l, u, icl);
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


/* circuit constraint */
void gecode_circuit_ivars_costs(CLSpace *space, IntArgs* c, IntVarArgs* x,
                                IntVarArgs* y, IntVar* z, IntConLevel icl) {
  circuit(*space, *c, *x, *y, *z, icl);
}

void gecode_circuit_ivars_costs_offset(CLSpace *space, IntArgs* c, int offset, 
                                       IntVarArgs* x, IntVarArgs* y, IntVar* z,
                                       IntConLevel icl) {
  circuit(*space, *c, offset, *x, *y, *z, icl);
}

void gecode_circuit_ivars(CLSpace *space, IntArgs* c, IntVarArgs* x,
                          IntVar* z, IntConLevel icl) {
  circuit(*space, *c, *x, *z, icl);
}

void gecode_circuit_ivars_offset(CLSpace *space, IntArgs* c, int offset, 
                                 IntVarArgs* x, IntVar* z, IntConLevel icl) {
  circuit(*space, *c, offset, *x, *z, icl);
}

/* Hamiltonian path constraint */
void gecode_path_ivars(CLSpace *space, IntVarArgs* x,
                       IntVar* s, IntVar* e, IntConLevel icl) {
  path(*space, *x, *s, *e, icl);
}

void gecode_path_ivars_offset(CLSpace *space, int offset, IntVarArgs* x,
                              IntVar* s, IntVar* e, IntConLevel icl) {
  path(*space, offset, *x, *s, *e, icl);
}

void gecode_path_ivars_edge_costs(CLSpace *space, IntArgs* c, IntVarArgs* x,
                                  IntVar* s, IntVar* e, IntVarArgs* y,
                                  IntVar* z, IntConLevel icl) {
  path(*space, *c, *x, *s, *e, *y, *z, icl);
}

void gecode_path_ivars_edge_costs_offset(CLSpace *space, IntArgs* c, int offset,
                                         IntVarArgs* x, IntVar* s, IntVar* e,
                                         IntVarArgs* y, IntVar* z, IntConLevel icl) {
  path(*space, *c, offset, *x, *s, *e, *y, *z, icl);
}

void gecode_path_ivars_costs(CLSpace *space, IntArgs* c, IntVarArgs* x,
                             IntVar* s, IntVar* e, IntVar* z,
                             IntConLevel icl) {
  path(*space, *c, *x, *s, *e, *z, icl);
}

void gecode_path_ivars_costs_offset(CLSpace *space, IntArgs* c, int offset,
                                    IntVarArgs* x, IntVar* s, IntVar* e,
                                    IntVar* z, IntConLevel icl) {
  path(*space, *c, offset, *x, *s, *e, *z, icl);
}


/* DFA / extensional constraint */
DFA* gecode_DFA_create(int s, DFA::Transition* trns, int* f) {
  return new DFA(s, trns, f, true);
}
void gecode_DFA_delete(DFA* d) { delete d; }

void gecode_extensional_ivars_dfa(CLSpace* space, IntVarArgs* x, DFA* d,
                                  IntConLevel icl) {
  extensional(*space, *x, *d, icl);
}

void gecode_extensional_bvars_dfa(CLSpace* space, BoolVarArgs* x, DFA* d,
                                  IntConLevel icl) {
  extensional(*space, *x, *d, icl);
}

TupleSet* gecode_TupleSet_create(void) {
  return new TupleSet();
}
void gecode_TupleSet_delete(TupleSet* d) { delete d; }

void gecode_TupleSet_add(TupleSet* d, IntArgs* i) {
  EXCSTART
  d->add(*i);
  EXCSTOP
}

int gecode_TupleSet_count(TupleSet* d) {
  d->finalize();
  return d->tuples();
}

void gecode_extensional_ivars_tset(CLSpace* space, IntVarArgs* x, TupleSet* d,
                                   ExtensionalPropKind epk, IntConLevel icl) {
  extensional(*space, *x, *d, epk, icl);
}

void gecode_extensional_bvars_tset(CLSpace* space, BoolVarArgs* x, TupleSet* d,
                                   ExtensionalPropKind epk, IntConLevel icl) {
  extensional(*space, *x, *d, epk, icl);
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
void gecode_dom_fvar_dbl(CLSpace *space, FloatVar* x, double n) {
  dom(*space, *x, FloatNum(n));
}

void gecode_dom_fvars_dbl(CLSpace *space, FloatVarArgs* x, double n) {
  dom(*space, *x, FloatNum(n));
}

void gecode_dom_fvar_dbl_dbl(CLSpace *space, FloatVar* x, double l, double m) {
  dom(*space, *x, FloatNum(l), FloatNum(m));
}

void gecode_dom_fvars_dbl_dbl(CLSpace *space, FloatVarArgs* x, double l, double u) {
  dom(*space, *x, FloatNum(l), FloatNum(u));
}

void gecode_dom_fvar_dbl_reified(CLSpace *space, FloatVar* x, double n,
                                 ReifyMode mode, BoolVar* bvar) {
  dom(*space, *x, FloatNum(n), Reify(*bvar, mode));
}

void gecode_dom_fvar_dbl_dbl_reified(CLSpace *space, FloatVar* x, double l, double m,
                                     ReifyMode mode, BoolVar* bvar) {
  dom(*space, *x, FloatNum(l), FloatNum(m), Reify(*bvar, mode));
}

void gecode_dom_fvar_fvar(CLSpace *space, FloatVar* x, FloatVar* d) {
  dom(*space, *x, *d);
}

void gecode_dom_fvars_fvars(CLSpace *space, FloatVarArgs* x, FloatVarArgs* d) {
  dom(*space, *x, *d);
}


void gecode_rel_fvar_fvar(CLSpace *space, FloatRelType op, FloatVar* x0, FloatVar* x1) {
  rel(*space, *x0, op, *x1); 
}

void gecode_rel_fvar_dbl(CLSpace *space, FloatRelType op, FloatVar* x0, double x1) {
  rel(*space, *x0, op, FloatNum(x1)); 
}

void gecode_rel_fvar_dbl_reified(CLSpace *space, FloatRelType op, FloatVar* x0, double x1,
				 ReifyMode mode, BoolVar* bvar) {
  rel(*space, *x0, op, FloatNum(x1), Reify(*bvar, mode)); 
}

void gecode_rel_fvar_fvar_reified(CLSpace *space, FloatRelType op, FloatVar* x0, FloatVar* x1,
				  ReifyMode mode, BoolVar* bvar) {
  rel(*space, *x0, op, *x1, Reify(*bvar, mode)); 
}

void gecode_rel_fvars_dbl(CLSpace *space, FloatRelType op, FloatVarArgs* x0, double x1) {
  rel(*space, *x0, op, FloatNum(x1)); 
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

void gecode_pow_fvar_uint_fvar(CLSpace *space, FloatVar* x0, int pow, FloatVar* x1) {
  Gecode::pow(*space, *x0, pow, *x1); 
}

void gecode_nroot_fvar_uint_fvar(CLSpace *space, FloatVar* x0, int pow, FloatVar* x1) {
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

void gecode_lin_fargs_fvars_dbl(CLSpace *space, FloatRelType rel, FloatValArgs* a, FloatVarArgs* x,
				double c) {
  linear(*space, *a, *x, rel, FloatNum(c));
}

void gecode_lin_fargs_fvars_fvar(CLSpace *space, FloatRelType rel, FloatValArgs* a, FloatVarArgs* x,
				 FloatVar* y) {
  linear(*space, *a, *x, rel, *y);
}

void gecode_lin_fargs_fvars_dbl_reified(CLSpace *space, FloatRelType rel, FloatValArgs* a,
					FloatVarArgs* x, double c,
					ReifyMode mode, BoolVar* bvar) {
  linear(*space, *a, *x, rel, FloatNum(c), Reify(*bvar, mode));
}

void gecode_lin_fargs_fvars_fvar_reified(CLSpace *space, FloatRelType rel, FloatValArgs* a,
					 FloatVarArgs* x, FloatVar* y,
					 ReifyMode mode, BoolVar* bvar) {
  linear(*space, *a, *x, rel, *y, Reify(*bvar, mode));
}

void gecode_channel_fvar_ivar(CLSpace *space, FloatVar* x0, IntVar* x1) {
  channel(*space, *x0, *x1);
}

// Float Branchers
BrancherHandle* gecode_branch_fvar(CLSpace *space, FloatVar* var, FloatValBranch* valb) {
  return new BrancherHandle(branch(*space, *var, *valb));
}

BrancherHandle* gecode_branch_fvars(CLSpace *space, FloatVarArgs* vars,
                                    FloatVarBranch* varb, FloatValBranch* valb) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb));
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
FloatVarBranch* FLOAT_VAR_DEGREE_SIZE_MIN(void){
  // Gecode::FLOAT_VAR_DEGREE_SIZE_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_SIZE_MAX, NULL);
}
FloatVarBranch* FLOAT_VAR_DEGREE_SIZE_MAX(void){
  // Gecode::FLOAT_VAR_DEGREE_SIZE_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_DEGREE_SIZE_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_AFC_SIZE_MIN(void){
  // Gecode::FLOAT_VAR_AFC_SIZE_MIN(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_AFC_SIZE_MIN, NULL);
}
FloatVarBranch* FLOAT_VAR_AFC_SIZE_MAX(void){
  // Gecode::FLOAT_VAR_AFC_SIZE_MAX(NULL);
  return new FloatVarBranch(FloatVarBranch::SEL_AFC_SIZE_MAX, NULL);
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


/* set domain constraints */
void gecode_dom_svar_int(CLSpace *space, SetRelType r, SetVar* x, int i) {
  dom(*space, *x, r, i);
}

void gecode_dom_svars_int(CLSpace *space, SetRelType r, SetVarArgs* x, int i) {
  dom(*space, *x, r, i);
}

void gecode_dom_svar_int_int(CLSpace *space, SetRelType r, SetVar* x,
                             int i, int j) {
  dom(*space, *x, r, i, j);
}

void gecode_dom_svars_int_int(CLSpace *space, SetRelType r, SetVarArgs* x,
                              int i, int j) {
  dom(*space, *x, r, i, j);
}


void gecode_dom_svar_iset(CLSpace *space, SetRelType r, SetVar* x, IntSet* s) {
  dom(*space, *x, r, *s);
}

void gecode_dom_svars_iset(CLSpace *space, SetRelType r, SetVarArgs* x, IntSet* s) {
  dom(*space, *x, r, *s);
}

void gecode_cardinality_svar_uint_uint(CLSpace *space, SetVar* x,
                                       unsigned int i, unsigned int j) {
  cardinality(*space, *x, i, j);
}

void gecode_cardinality_svars_uint_uint(CLSpace *space, SetVarArgs* x,
                                        unsigned int i, unsigned int j) {
  cardinality(*space, *x, i, j);
}

void gecode_cardinality_svar_ivar(CLSpace *space, SetVar* x, IntVar* i) {
  cardinality(*space, *x, *i);
}

void gecode_dom_svar_int_reified(CLSpace *space, SetRelType r, SetVar* x,
                                 int i, ReifyMode mode, BoolVar* b) {
  dom(*space, *x, r, i, Reify(*b, mode));
}

void gecode_dom_svar_int_int_reified(CLSpace *space, SetRelType r, SetVar* x,
                             int i, int j, ReifyMode mode, BoolVar* b) {
  dom(*space, *x, r, i, j, Reify(*b, mode));
}

void gecode_dom_svar_iset_reified(CLSpace *space, SetRelType r, SetVar* x,
                                  IntSet* i, ReifyMode mode, BoolVar* b) {
  dom(*space, *x, r, *i, Reify(*b, mode));
}

void gecode_dom_svar_svar(CLSpace *space, SetVar* x, SetVar* d) {
  dom(*space, *x, *d);
}

void gecode_dom_svars_svars(CLSpace *space, SetVarArgs* x, SetVarArgs* d) {
  dom(*space, *x, *d);
}


/* set relations */
void gecode_srel_svar_svar(CLSpace *space, SetVar* x, SetRelType r, SetVar* y) {
  rel(*space, *x, r, *y);
}

void gecode_srel_svar_svar_reified(CLSpace *space, SetVar* x, SetRelType r, SetVar* y,
                                   ReifyMode mode, BoolVar* b) {
  rel(*space, *x, r, *y, Reify(*b, mode));
}

void gecode_srel_svar_ivar(CLSpace *space, SetVar* x, SetRelType r, IntVar* y) {
  rel(*space, *x, r, *y);
}

void gecode_srel_ivar_svar(CLSpace *space, IntVar* x, SetRelType r, SetVar* y) {
  rel(*space, *x, r, *y);
}

void gecode_srel_svar_ivar_reified(CLSpace *space, SetVar* x, SetRelType r, IntVar* y,
                                   ReifyMode mode, BoolVar* b) {
  rel(*space, *x, r, *y, Reify(*b, mode));
}

void gecode_srel_ivar_svar_reified(CLSpace *space, IntVar* x, SetRelType r, SetVar* y,
                                   ReifyMode mode, BoolVar* b) {
  rel(*space, *x, r, *y, Reify(*b, mode));
}

void gecode_irel_svar_ivar(CLSpace *space, SetVar* x, IntRelType r, IntVar* y) {
  rel(*space, *x, r, *y);
}

void gecode_irel_ivar_svar(CLSpace *space, IntVar* x, IntRelType r, SetVar* y) {
  rel(*space, *x, r, *y);
}

/* set operations */
void gecode_sop_svar_svar_srel_svar(CLSpace *space, SetVar* x, SetOpType op, SetVar* y,
                                    SetRelType r, SetVar* z) {
  rel(*space, *x, op, *y, r, *z);
}

void gecode_sop_svars_eql_svar(CLSpace *space, SetOpType op, SetVarArgs* x,
                               SetVar* y) {
  rel(*space, op, *x, *y);
}

void gecode_sop_svars_iset_eql_svar(CLSpace *space, SetOpType op, SetVarArgs* x,
                                    IntSet* z, SetVar* y) {
  rel(*space, op, *x, *z, *y);
}

void gecode_sop_ivars_iset_eql_svar(CLSpace *space, SetOpType op, IntVarArgs* x,
                                    IntSet* z, SetVar* y) {
  rel(*space, op, *x, *z, *y);
}

void gecode_sop_ivars_eql_svar(CLSpace *space, SetOpType op, IntVarArgs* x,
                               SetVar* y) {
  rel(*space, op, *x, *y);
}

void gecode_sop_iset_svar_srel_svar(CLSpace *space, IntSet* x, SetOpType op, SetVar* y,
                                    SetRelType r, SetVar* z) {
  rel(*space, *x, op, *y, r, *z);
}

void gecode_sop_svar_iset_srel_svar(CLSpace *space, SetVar* x, SetOpType op, IntSet* y,
                                    SetRelType r, SetVar* z) {
  rel(*space, *x, op, *y, r, *z);
}

void gecode_sop_svar_svar_srel_iset(CLSpace *space, SetVar* x, SetOpType op, SetVar* y,
                                    SetRelType r, IntSet* z) {
  rel(*space, *x, op, *y, r, *z);
}

void gecode_sop_iset_svar_srel_iset(CLSpace *space, IntSet* x, SetOpType op, SetVar* y,
                                    SetRelType r, IntSet* z) {
  rel(*space, *x, op, *y, r, *z);
}

void gecode_sop_svar_iset_srel_iset(CLSpace *space, SetVar* x, SetOpType op, IntSet* y,
                                    SetRelType r, IntSet* z) {
  rel(*space, *x, op, *y, r, *z);
}


/* convex hull constraint */
void gecode_convex_svar(CLSpace *space, SetVar* x) {
  convex(*space, *x);
}

void gecode_convex_svar_svar(CLSpace *space, SetVar* x, SetVar* y) {
  convex(*space, *x, *y);
}

/* sequence constraint */
void gecode_sequence_svars(CLSpace *space, SetVarArgs* x) {
  sequence(*space, *x);
}

void gecode_sequence_svars_svar(CLSpace *space, SetVarArgs* y, SetVar* x) {
  sequence(*space, *y, *x);
}

/* distinctness constraint*/
void gecode_atmostOne_svars(CLSpace *space, SetVarArgs* x, unsigned int c) {
  atmostOne(*space, *x , c);
}

/* connection constraints */
void gecode_min_svar_ivar(CLSpace *space, SetVar* x, IntVar* i) {
  min(*space, *x, *i);
}

void gecode_notMin_svar_ivar(CLSpace *space, SetVar* x, IntVar* i) {
  notMin(*space, *x, *i);
}

void gecode_min_svar_ivar_reified(CLSpace *space, SetVar* x, IntVar* i,
                                  ReifyMode mode, BoolVar* b) {
  min(*space, *x, *i, Reify(*b, mode));
}

void gecode_max_svar_ivar(CLSpace *space, SetVar* x, IntVar* i) {
  max(*space, *x, *i);
}

void gecode_notMax_svar_ivar(CLSpace *space, SetVar* x, IntVar* i) {
  notMax(*space, *x, *i);
}

void gecode_max_svar_ivar_reified(CLSpace *space, SetVar* x, IntVar* i,
                                  ReifyMode mode, BoolVar* b) {
  max(*space, *x, *i, Reify(*b, mode));
}

void gecode_weights_ints_ints_svar_ivar(CLSpace *space, IntArgs* elements,
                                        IntArgs* weights, SetVar* x, IntVar* y) {
  Gecode::weights(*space, *elements, *weights, *x, *y);
}

/* channel constraint */
void gecode_channel_ivars_svars(CLSpace *space, IntVarArgs* x, SetVarArgs* y) {
  channel(*space, *x, *y);
}

void gecode_channel_bvars_svar(CLSpace *space, BoolVarArgs* x, SetVar* y) {
  channel(*space, *x, *y);
}

void gecode_channel_svars_svars(CLSpace *space, SetVarArgs* x, SetVarArgs* y) {
  channel(*space, *x, *y);
}

void gecode_channelSorted_ivars_svar(CLSpace *space, IntVarArgs* x, SetVar* y) {
  channelSorted(*space, *x, *y);
}

/* precede constraint */
void gecode_precede_svars_int_int(CLSpace *space, SetVarArgs* x, int s, int t) {
  precede(*space, *x, s, t);
}

void gecode_precede_svars_ints(CLSpace *space, SetVarArgs* x, IntArgs* c) {
  precede(*space, *x, *c);
}

/* element constraint */
void gecode_element_svars_svar_svar(CLSpace *space, SetOpType op, SetVarArgs* x,
                                    SetVar* y, SetVar* z) {
  element(*space, op, *x, *y, *z);
}
                                 
void gecode_element_ivars_svar_svar(CLSpace *space, SetOpType op, IntVarArgs* x,
                                    SetVar* y, SetVar* z) {
  element(*space, op, *x, *y, *z);
}

void gecode_element_isets_svar_svar(CLSpace *space, SetOpType op, IntSetArgs* x,
                                    SetVar* y, SetVar* z) {
  element(*space, op, *x, *y, *z);
}

void gecode_element_ints_svar_svar(CLSpace *space, SetOpType op, IntArgs* x,
                                   SetVar* y, SetVar* z) {
  element(*space, op, *x, *y, *z);
}


void gecode_element_svars_ivar_svar(CLSpace *space,
                                    SetVarArgs* x, IntVar* y, SetVar* z) {
  element(*space, *x, *y, *z);
}

void gecode_element_isets_ivar_svar(CLSpace *space,
                                    IntSetArgs* x, IntVar* y, SetVar* z) {
  element(*space, *x, *y, *z);
}

void gecode_element_isets_ivar_int_ivar_int_svar(CLSpace *space, IntSetArgs* a, 
                                                 IntVar* x, int w,
                                                 IntVar* y, int h,
                                                 SetVar* z) {
  element(*space, *a, *x, w, *y, h, *z);
}

void gecode_element_svars_ivar_int_ivar_int_svar(CLSpace *space, SetVarArgs* a, 
                                                 IntVar* x, int w,
                                                 IntVar* y, int h,
                                                 SetVar* z) {
  element(*space, *a, *x, w, *y, h, *z);
}


/* set branchers */
void gecode_svar_selector_delete(SetVarBranch* s){
  delete s;
}
void gecode_sval_selector_delete(SetValBranch* s){
  delete s;
}

BrancherHandle* gecode_branch_svar(CLSpace *space, SetVar* var, SetValBranch* valb) {
  return new BrancherHandle(branch(*space, *var, *valb));
}

BrancherHandle* gecode_branch_svars(CLSpace *space, SetVarArgs* vars,
                                    SetVarBranch* varb, SetValBranch* valb) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb));
}

// Symmetries
SymmetryHandle* gecode_VariableSymmetry_svars(SetVarArgs* x) {
  return new SymmetryHandle(VariableSymmetry(*x));
}

SymmetryHandle* gecode_VariableSequenceSymmetry_svars(SetVarArgs* x,
                                                      int ss) {
  return new SymmetryHandle(VariableSequenceSymmetry(*x, ss));
}

BrancherHandle* gecode_branch_svars_sym(CLSpace *space, SetVarArgs* vars,
                                        SetVarBranch* varb, SetValBranch* valb,
                                        Symmetries* sym) {
  return new BrancherHandle(branch(*space, *vars, *varb, *valb, *sym));
}


SetVarBranch* SET_VAR_NONE(void){
  // Gecode::SET_VAR_NONE();
  return new SetVarBranch(SetVarBranch::SEL_NONE, NULL);
}
SetVarBranch* SET_VAR_RND(unsigned int seed){
  // Gecode::SET_VAR_RND(Rnd(seed));
  return new SetVarBranch(Rnd(seed));
}
// SetVarBranch SET_VAR_MERIT_MIN(SetBranchMerit bm, BranchTbl tbl=NULL);
// SetVarBranch SET_VAR_MERIT_MAX(SetBranchMerit bm, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_DEGREE_MIN(void){
  return new SetVarBranch(SetVarBranch::SEL_DEGREE_MIN, NULL);
}
SetVarBranch* SET_VAR_DEGREE_MAX(void){
  return new SetVarBranch(SetVarBranch::SEL_DEGREE_MAX, NULL);
}
SetVarBranch* SET_VAR_AFC_MIN(double d){
  return new SetVarBranch(SetVarBranch::SEL_AFC_MIN, d, NULL);
}
// SetVarBranch SET_VAR_AFC_MIN(SetAFC a, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_AFC_MAX(double d){
  return new SetVarBranch(SetVarBranch::SEL_AFC_MIN, d, NULL);
}
// SetVarBranch SET_VAR_AFC_MAX(SetAFC a, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_ACTIVITY_MIN(double d){
  return new SetVarBranch(SetVarBranch::SEL_ACTIVITY_MIN, d, NULL);
}
// SetVarBranch SET_VAR_ACTIVITY_MIN(SetActivity a, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_ACTIVITY_MAX(double d){
  return new SetVarBranch(SetVarBranch::SEL_ACTIVITY_MAX, d, NULL);
}
// SetVarBranch SET_VAR_ACTIVITY_MAX(SetActivity a, BranchTbl tbl=NULL);     
SetVarBranch* SET_VAR_MIN_MIN(void){
  return new SetVarBranch(SetVarBranch::SEL_MIN_MIN, NULL);
}
SetVarBranch* SET_VAR_MIN_MAX(void){
  return new SetVarBranch(SetVarBranch::SEL_MIN_MAX, NULL);
}
SetVarBranch* SET_VAR_MAX_MIN(void){
  return new SetVarBranch(SetVarBranch::SEL_MAX_MIN, NULL);
}
SetVarBranch* SET_VAR_MAX_MAX(void){
  return new SetVarBranch(SetVarBranch::SEL_MAX_MAX, NULL);
}
SetVarBranch* SET_VAR_SIZE_MIN(void){
  return new SetVarBranch(SetVarBranch::SEL_SIZE_MIN, NULL);
}
SetVarBranch* SET_VAR_SIZE_MAX(void){
  return new SetVarBranch(SetVarBranch::SEL_SIZE_MAX, NULL);
}
SetVarBranch* SET_VAR_DEGREE_SIZE_MIN(void){
  return new SetVarBranch(SetVarBranch::SEL_DEGREE_SIZE_MIN, NULL);
}
SetVarBranch* SET_VAR_DEGREE_SIZE_MAX(void){
  return new SetVarBranch(SetVarBranch::SEL_DEGREE_SIZE_MAX, NULL);
}
SetVarBranch* SET_VAR_AFC_SIZE_MIN(double d){
  return new SetVarBranch(SetVarBranch::SEL_AFC_SIZE_MIN, d, NULL);
}
// SetVarBranch SET_VAR_AFC_SIZE_MIN(SetAFC a, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_AFC_SIZE_MAX(double d){
  return new SetVarBranch(SetVarBranch::SEL_AFC_SIZE_MIN, d, NULL);
}
// SetVarBranch SET_VAR_AFC_SIZE_MAX(SetAFC a, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_ACTIVITY_SIZE_MIN(double d){
  return new SetVarBranch(SetVarBranch::SEL_ACTIVITY_SIZE_MIN, d, NULL);
}
// SetVarBranch SET_VAR_ACTIVITY_SIZE_MIN(SetActivity a, BranchTbl tbl=NULL);
SetVarBranch* SET_VAR_ACTIVITY_SIZE_MAX(double d){
  return new SetVarBranch(SetVarBranch::SEL_ACTIVITY_SIZE_MAX, d, NULL);
}
// SetVarBranch SET_VAR_ACTIVITY_SIZE_MAX(SetActivity a, BranchTbl tbl=NULL);     

SetValBranch* SET_VAL_MIN_INC(void){
  // Gecode::SET_VAL_MIN_INC();
  return new SetValBranch(SetValBranch::SEL_MIN_INC);
}
SetValBranch* SET_VAL_MIN_EXC(void){
  // Gecode::SET_VAL_MIN_EXC();
  return new SetValBranch(SetValBranch::SEL_MIN_EXC);
}
SetValBranch* SET_VAL_MED_INC(void){
  // Gecode::SET_VAL_MED_INC();
  return new SetValBranch(SetValBranch::SEL_MED_INC);
}
SetValBranch* SET_VAL_MED_EXC(void){
  // Gecode::SET_VAL_MED_EXC();
  return new SetValBranch(SetValBranch::SEL_MED_EXC);
}
SetValBranch* SET_VAL_MAX_INC(void){
  // Gecode::SET_VAL_MAX_INC();
  return new SetValBranch(SetValBranch::SEL_MAX_INC);
}
SetValBranch* SET_VAL_MAX_EXC(void){
  // Gecode::SET_VAL_MAX_EXC();
  return new SetValBranch(SetValBranch::SEL_MAX_EXC);
}
SetValBranch* SET_VAL_RND_INC(unsigned int seed){
  // Gecode::SET_VAL_RND_INC();
  return new SetValBranch(SetValBranch::SEL_RND_INC, Rnd(seed));
}
SetValBranch* SET_VAL_RND_EXC(unsigned int seed){
  // Gecode::SET_VAL_RND_EXC();
  return new SetValBranch(SetValBranch::SEL_RND_EXC, Rnd(seed));
}



} /* extern "C" */
