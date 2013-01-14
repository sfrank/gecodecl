%module gecodeglue

%insert("lisphead") %{
(in-package :gecodecl)
%}

%typemap(cin) bool ":boolean";
%typemap(cout) bool ":boolean";

%typemap(cin) size_t "size";
%typemap(cout) size_t "size";

%typemap(cin) BoolVar* "boolvar-type";
%typemap(cin) IntVar* "intvar-type";
%typemap(cin) FloatVar* "floatvar-type";

%typemap(cin) IntVarBranch*  "intvarselector-type";
%typemap(cout) IntVarBranch*  "intvarselector-type";
%typemap(cin) IntValBranch*  "intvalselector-type";
%typemap(cout) IntValBranch*  "intvalselector-type";

%typemap(cin) FloatVarBranch*  "floatvarselector-type";
%typemap(cout) FloatVarBranch*  "floatvarselector-type";
%typemap(cin) FloatValBranch*  "floatvalselector-type";
%typemap(cout) FloatValBranch*  "floatvalselector-type";


%typemap(cin) TaskTypeArgs* "tasktypeargs-type";

%typemap(cin) CLSpace* "space-type";
%typemap(cin) DFS<CLSpace>* "search-type";
%typemap(cin) BAB<CLSpace>* "search-type";
//%typemap(cin) RBS<CLSpace>* "search-type";

%typemap(cout) STATUS  "variable-status";

%typemap(cin) ReifyMode  "reify-mode";

%typemap(cin) IntVarArgs*  "intvarargs-type";
%typemap(cin) IntArgs*  "intargs-type";
%typemap(cin) BoolVarArgs*  "boolvarargs-type";
%typemap(cin) FloatVarArgs*  "floatvarargs-type";
%typemap(cin) FloatArgs*  "floatargs-type";
%typemap(cin) IntSetArgs*  "intsetargs-type";

%typemap(cin) IntRelType  "int-relation-type";
%typemap(cin) BoolOpType  "bool-operation-type";
%typemap(cin) IntConLevel "int-consistency-level";
%typemap(cin) FloatRelType  "float-relation-type";

%typemap(cin)  IntSet*  "intset-type";
%typemap(cout) IntSet*  "intset-type";

%typemap(cin)  DFA::Transition*  "(:pointer (:struct transition))";
%typemap(cin)  DFA*  "dfa-type";
%typemap(cout) DFA*  "dfa-type";


// %feature("intern_function","1");

%include "gecodeglue.h"

