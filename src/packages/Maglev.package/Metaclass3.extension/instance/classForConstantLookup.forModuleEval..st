*maglev-runtime
classForConstantLookup: envId forModuleEval: isModuleEvalBool
  isModuleEvalBool ifTrue:[  "fix Trac 484"
	 self isMeta ifTrue:[ ^ self ].
	 ^ self class  
  ] ifFalse:[
    self isMeta ifTrue:[ ^ destClass ].
    ^ self
  ].

