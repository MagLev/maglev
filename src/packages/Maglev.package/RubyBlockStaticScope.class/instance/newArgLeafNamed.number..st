*maglev-runtime
newArgLeafNamed: aSym number: aNumber
  | res blkNod ofs  | 
  blkNod := self irBlockNode .
  ofs := aNumber < 0 ifTrue:[ blkNod numArgs + 1 ] ifFalse:[aNumber].
  (res := GsComVarLeaf new)
			blockArg: aSym 
			argNumber: ofs 
			forBlock: blkNod  .
  TraceLocals >= 1 ifTrue:[
	 self trace: ' newArgLeaf ', aNumber asString , ' created varLeaf blockArg for ', aSym.  
  ].
  ^ res 

