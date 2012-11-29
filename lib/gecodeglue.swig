%module gecodeglue

%insert("lisphead") %{
(in-package :gecodecl)
%}

%typemap(cin) size_t "size";
%typemap(cout) size_t "size";

%typemap(cin) CLSpace* "space-type";

%typemap(cout) STATUS  "variable-status";

%typemap(cin) ReifyMode  "reify-mode";

// %typemap(cin) IntVarBranch  "bvar-sel";
// %typemap(cin) IntValBranch  "bval-sel";

%typemap(cin) IntRelType  "int-relation-type";
%typemap(cin) BoolOpType  "bool-operation-type";
%typemap(cin) IntConLevel "int-consistency-level";

// %feature("intern_function","1");

%include "gecodeglue.h"

