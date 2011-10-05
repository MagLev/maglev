
doit
RubyGlobalVarNode subclass: 'RubyGlobalLastException'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalLastException
removeallmethods
removeallclassmethods

set class RubyGlobalLastException
category: 'as yet unclassified'
method:
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

%

