
set class RubyReturnNode class
category: '*maglev-ast'
method:
s_a: val b: srcOfs
  | res |
  (res := self _basicNew)
     valueNode: val ; position: srcOfs  .
  ^ res

%


set class RubyReturnNode
category: 'converting'
method:
irNode
	| value |  
	value := valueNode ifNil: [ GsComLiteralNode newNil ] 
	                  ifNotNil: [valueNode irReturnNode ].
	^ self ir: (GsComReturnNode new returnFromHome: value)

%


set class RubyReturnNode
category: '*maglev-ast'
method:
is_void_result
  ^ true

%


set class RubyReturnNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: 'return ';
		printNode: valueNode

%


set class RubyReturnNode
category: 'accessing'
method:
valueNode

	 ^ valueNode

%


set class RubyReturnNode
category: 'accessing'
method:
valueNode: aNode
	valueNode := aNode

%


set class RubyReturnNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  valueNode walkWithScope: aScope

%


set class RubyReturnNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:return, ', valueNode _inspect  , $]

%

