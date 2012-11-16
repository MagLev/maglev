*maglev-runtime
walkWithScope: aScope
  | cst tmd |
  (rcvrIsSelf := rcvrNode class == RubySelfNode ) ifTrue:[
    (tmd := (cst := RubyCompilerState current) topMethodDef) setHasBlockArgRef ifTrue:[
      "inEval"  evalRcvr := true .
    ].
  ].
  super walkWithScope: aScope 

