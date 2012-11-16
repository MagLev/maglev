as yet unclassified
irNode 
  | aLeaf node |
  aLeaf := RubyCompilerState current lastExceptionStack topOrNil .
  aLeaf ifNil:[   
	"handle access to ruby global $!  outside of a rescue clause "  
	^ super irNode .
  ].
  node := GsComVariableNode new leaf: aLeaf .
  self ir: node .
  ^ node

