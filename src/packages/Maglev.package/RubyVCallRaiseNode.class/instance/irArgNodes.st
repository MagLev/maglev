as yet unclassified
irArgNodes 
  "return  the leaf for $! , arg to the currently active exception handler block, or none"
  | aLeaf |
  aLeaf := RubyCompilerState current lastExceptionStack topOrNil .
  aLeaf ifNotNil:[ 
	  irArg ifNotNil:[ self error:' irArgNodes being recomputed' ].
	  irArg := GsComVariableNode new leaf: aLeaf .
	  ^ { irArg } 
  ].
  ^ #() 

