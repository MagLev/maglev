*maglev-runtime
walkWithScope: aScope 
   | mth anod | 
   mth := RubyCompilerState current topMethodDefOrNil .
   mth ifNil:[ self error: 'no enclosing method def for yield' ].
   mth setHasBlockArgRef ifTrue:[ "inEval"  evalRcvr := true ].

   (anod := argsNode) ifNotNil:[
     anod isSplatNode ifTrue:[
       anod walkWithScope: aScope  "because SplatNode>>list  lies "
     ] ifFalse:[
        super walkWithScope: aScope  "because we will use super.irArgNodes"
     ].
   ] ifNil:[
     super walkWithScope: aScope  "because we will use super.irArgNodes"
   ].
   "YieldNode has no receiverNode , so does not need to postWalkForYield "

