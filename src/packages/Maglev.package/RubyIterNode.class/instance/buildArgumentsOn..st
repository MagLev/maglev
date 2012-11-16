as yet unclassified
buildArgumentsOn: irBlock
   | argNod maNod |
   (argNod := varNode) ifNil:[ 
	  zeroDeclaredArgs ifNil:[ irBlock setNoArgsDecl  "for  Proc.new { }.arity == -1" ]	
   ] ifNotNil:[ 
	  argNod buildBlockArgumentsOn: irBlock .  "a DAsgn or ParAsgn"
   ].

