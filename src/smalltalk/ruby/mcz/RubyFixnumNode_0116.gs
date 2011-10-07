
set class RubyFixnumNode
category: '*maglev-ast'
classmethod:
newForInt: anInt
  ^ self _basicNew _value: anInt 

%


set class RubyFixnumNode
category: 'as yet unclassified'
classmethod:
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

