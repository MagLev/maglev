as yet unclassified
addProc: aProc
  "a Ruby primitive"
  | theProcs |
  1"__callerEnvId" == 1 ifFalse:[ ArgumentError signal:'finalization only implemented for env 1'].
  (theProcs := procs) ifNil:[ ArgumentError signal:'finalization has already executed'].
  theProcs add: aProc .
  

