
doit
RubyNode subclass: 'RubySValueNode'
	instVarNames: #( node)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySValueNode
removeallmethods
removeallclassmethods

set class RubySValueNode class
category: '*maglev-ast'
method:
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

