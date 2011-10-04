
doit
RubyAbstractLiteralNode subclass: 'RubyTrueNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyTrueNode
removeallmethods
removeallclassmethods

set class RubyTrueNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #'true'

%


set class RubyTrueNode
category: 'converting'
method:
irLeaf
	^ self ir: (GsComLitLeaf new specialLiteral: true)

%


set class RubyTrueNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: 'true'

%


set class RubyTrueNode
category: '*maglev-runtime'
method:
_inspect
  ^ ':true' 

%

