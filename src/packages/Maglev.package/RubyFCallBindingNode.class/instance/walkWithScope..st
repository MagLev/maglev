*maglev-runtime
walkWithScope: aScope
   "MRI parse server only: produces FCall nodes with zero args for calls
     with empty parenthesis" 
   self argsNode size ~~ 0 ifTrue:[   "fix Trac543"
     self error:' binding with arguments not supported' 
   ].
   RubyCompilerState current topMethodDef setSendsBinding ; setHasInnerEvalOrDef .
   ^ super walkWithScope: aScope 

