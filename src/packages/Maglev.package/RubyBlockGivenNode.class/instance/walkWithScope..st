*maglev-runtime
walkWithScope: aScope
  RubyCompilerState current topMethodDef setHasBlockArgRef ifTrue:[
    "inEval" evalRcvr := true .
  ].
  super walkWithScope: aScope 

