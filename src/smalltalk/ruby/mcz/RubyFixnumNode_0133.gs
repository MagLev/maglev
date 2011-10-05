
doit
RubyAbstractNumberNode subclass: 'RubyFixnumNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFixnumNode
removeallmethods
removeallclassmethods

set class RubyFixnumNode class
category: '*maglev-ast'
method:
newForInt: anInt
  ^ self _basicNew _value: anInt 

%


set class RubyFixnumNode class
category: 'as yet unclassified'
method:
newForIr

  ^ self _basicNew "position left nil"

%


set class RubyFixnumNode
category: 'converting'
method:
irLeaf
	^ self ir: (GsComLitLeaf new integerLiteral:  value)

%


set class RubyFixnumNode
category: 'parsetree-test'
method:
isSameAs: aRubyNode
	^ value = aRubyNode value

%


set class RubyFixnumNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: value asString

%


set class RubyFixnumNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:lit, ', value _inspect , $]

%


set class RubyFixnumNode
category: '*maglev-ast'
method:
_value
  ^ value

%


set class RubyFixnumNode
category: '*maglev-ast'
method:
_value: aSmallInteger
  value := aSmallInteger

%

