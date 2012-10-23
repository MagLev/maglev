as yet unclassified
initializeForModule: aModule persistentCopy: pns  env: envArg
  self persistentCopy: pns .
  self initializeForModule: aModule  env: envArg .
  pns ifNotNil:[
	 moduleFrozen := pns moduleFrozen .
	 name := pns name .  "ensure use of override of smalltalk name"
  ].

