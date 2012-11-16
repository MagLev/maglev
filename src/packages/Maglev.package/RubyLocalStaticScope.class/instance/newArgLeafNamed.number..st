*maglev-runtime
newArgLeafNamed: aSym number: aNumber
  | res  | 
  (res := GsComVarLeaf new)
			methodArg: aSym 
			argNumber: aNumber .
  TraceLocals >= 1 ifTrue:[
	self trace: ' argLeafAtIndex:', aNumber asString , ' created varLeaf methArg for ', aSym
  ].
  ^ res 

