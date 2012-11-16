*maglev-runtime
walkWithScope: aScope
  RubyCompilerState current topMethodDef setSendsBinding ; setHasInnerEvalOrDef .
  ^ super walkWithScope: aScope 

