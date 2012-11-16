accessing
varNode: aNode 
   aNode == 0 ifTrue:[
	 varNode := nil .
	 zeroDeclaredArgs := true .
   ] ifFalse:[
	 varNode := aNode .
	 self setIsBlockArg: aNode
   ].

