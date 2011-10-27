
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

