
set class RubyMatch2Node class
category: '*maglev-ast'
method:
s_a: regex b: rcvr
  | res |
  (res := self _basicNew)
     receiverNode: rcvr ; valueNode: regex .
   ^ res

%


set class RubyMatch2Node
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := valueNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyMatch2Node
category: 'accessing'
method:
valueNode

	 ^ valueNode

%


set class RubyMatch2Node
category: 'accessing'
method:
valueNode: aNode
	valueNode := aNode

%


set class RubyMatch2Node
category: '*maglev-runtime'
method:
_inspect
  ^ '[:match2, ', receiverNode _inspect , ' , ' , valueNode _inspect, $]

%

