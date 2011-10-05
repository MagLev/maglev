
set class RubyOpAsgnOrNode class
category: '*maglev-ast'
method:
s_a: first b: second
^ self _basicNew firstNode: first secondNode: second 

%


set class RubyOpAsgnOrNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := secondNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyOpAsgnOrNode
category: 'as yet unclassified'
method:
definedQkind
  ^ 'assignment'

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
firstNode

	 ^ firstNode

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
firstNode: aNode
	firstNode := aNode

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
irArgNodes
	 ^ { self newBlock:
		[:block |
		block appendStatement: secondNode irArgNode.
		block ] }

%


set class RubyOpAsgnOrNode
category: 'as yet unclassified'
method:
irReceiverNode 
   ^ firstNode irEvaluatedOpOrRcvr

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
isSmalltalkSend
	^ true

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
receiverNode
	^ firstNode

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
secondNode

	 ^ secondNode

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
secondNode: aNode
	secondNode := aNode

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
selector
	^ #or:

%


set class RubyOpAsgnOrNode
category: 'accessing'
method:
shouldOptimize
	^ true

%


set class RubyOpAsgnOrNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:op_asgn_or, ', firstNode _inspect, ', ', secondNode _inspect , $]

%

