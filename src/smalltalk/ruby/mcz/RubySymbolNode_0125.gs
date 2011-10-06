
set class RubySymbolNode
category: 'as yet unclassified'
classmethod:
newForIr
  ^ self _basicNew "during IR phase, leave position nil"

%


set class RubySymbolNode
category: '*maglev-ast'
classmethod:
s_a: val
  ^ self _basicNew
     name: (val _isSymbol ifTrue:[ val ] ifFalse:[ val symval ])

%


set class RubySymbolNode
category: 'as yet unclassified'
method:
determineDynamic
   ^ nil " :asymbol::ACONST signals TypeError at runtime"

%


set class RubySymbolNode
category: 'converting'
method:
irLeaf
	^ self ir: (GsComLitLeaf new symbolLiteral: name)

%


set class RubySymbolNode
category: 'parsetree-test'
method:
isSameAs: other
	^ self name = other name

%


set class RubySymbolNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubySymbolNode
category: '*maglev-ast'
method:
name: aSymbol 
  aSymbol _isSymbol ifFalse:[ RubyParserM signalError:'RubySymbolNode value not a Symbol'].
  name := aSymbol 

%


set class RubySymbolNode
category: 'as yet unclassified'
method:
pathArray
   ^ nil "signals TypeError at runtime"

%


set class RubySymbolNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: ':', name

%


set class RubySymbolNode
category: '*maglev-ast'
method:
strNodeValue
   ^ name

%


set class RubySymbolNode
category: '*maglev-ast'
method:
symNodeValue
  ^ name

%


set class RubySymbolNode
category: '*maglev-runtime'
method:
_inspect
  ^ name _inspect 

%


set class RubySymbolNode
category: '*maglev-runtime'
method:
_value
  ^ name

%

