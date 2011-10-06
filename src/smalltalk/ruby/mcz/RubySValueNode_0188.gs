
set class RubySValueNode
category: '*maglev-ast'
classmethod:
s_a: body
 | res |
  (res := self _basicNew)
     node: body .
  ^ res

%


set class RubySValueNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ {node}

%


set class RubySValueNode
category: 'accessing'
method:
irLocalAsgnValue
	^ node irLocalAsgnValue

%


set class RubySValueNode
category: 'accessing'
method:
irNode
	^ node irNode

%


set class RubySValueNode
category: 'accessing'
method:
irReturnNode
	^ node irReturnNode

%


set class RubySValueNode
category: 'accessing'
method:
node

	 ^ node

%


set class RubySValueNode
category: 'accessing'
method:
node: aNode
	node := aNode

%


set class RubySValueNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
   node walkWithScope: aScope

%


set class RubySValueNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:svalue, ', node _inspect , $]

%

