as yet unclassified
new: anObject proc: aProc

"a ruby primitive"
1"__callerEnvId" == 1 ifFalse:[ ArgumentError signal:'finalization only implemented for env 1'].
anObject isCommitted ifTrue:[
  ArgumentError signal:'define_finalizer not allowed on committed object'
].
"kindOf checks on aProc done in Ruby caller."

^ self _basicNew obj: anObject proc: aProc 

