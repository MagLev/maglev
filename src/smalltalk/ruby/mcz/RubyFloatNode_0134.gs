
doit
RubyAbstractNumberNode subclass: 'RubyFloatNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFloatNode
removeallmethods
removeallclassmethods

set class RubyFloatNode
category: 'converting'
method:
irLeaf
	^ self ir: (GsComLitLeaf new floatLiteral: value)

%


set class RubyFloatNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: value asString

%


set class RubyFloatNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:lit, ', value _inspect , $]

%


set class RubyFloatNode
category: '*maglev-ast'
method:
_value
  ^ value

%


set class RubyFloatNode
category: '*maglev-ast'
method:
_value: aFloat
  value := aFloat

%

