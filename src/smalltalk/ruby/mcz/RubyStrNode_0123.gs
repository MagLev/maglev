
set class RubyStrNode
category: 'as yet unclassified'
classmethod:
newForIr

  ^ self _basicNew "position left nil"

%


set class RubyStrNode
category: '*maglev-ast'
classmethod:
s_a: val 
 | res |
  (res := self _basicNew)
     _value: val .
  ^ res

%


set class RubyStrNode
category: '*maglev-ast'
method:
appendString: aString
  value add: aString

%


set class RubyStrNode
category: '(as yet unclassified)'
method:
irLeaf
  ^ self ir: (GsComLitLeaf new rubyCopyingStringLiteral: value)

%


set class RubyStrNode
category: '(as yet unclassified)'
method:
isSameAs: aNode

  ^ value = aNode value

%


set class RubyStrNode
category: '(as yet unclassified)'
method:
printSourceOn: aStream
  aStream nextPut: $' ; 
   nextPutAll: value ;
   nextPut: $' 

%


set class RubyStrNode
category: '*maglev-ast'
method:
strNodeValue
  ^ value

%


set class RubyStrNode
category: '*maglev-ast'
method:
str_dstr_evstr_kind
  ^ 0

%


set class RubyStrNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:str, ', value _inspect  , $]

%


set class RubyStrNode
category: '*maglev-runtime'
method:
_value
  ^ value

%


set class RubyStrNode
category: '*maglev-ast'
method:
_value: v
  value := v 

%

